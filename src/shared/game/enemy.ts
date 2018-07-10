import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";
import { clearTemp } from "src/shared/game/crew";
import { ActionRest, Action, BattleTurn, doAction } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { Target } from "src/shared/game/target";
import { doAttack } from "src/shared/game/attack";

export type Enemy = {
  rank: number,
  actions: EnemyAttack[],
};

type MeleeAttack = {
  tag: "MeleeAttack",
  multiplier: number,
  positions: number[],
};

type Heal = {
  tag: "Heal",
  value: number,
};

export type EnemyAttack
  = MeleeAttack
  | Heal
  ;

function battleStep(
  state: GameState,
  enemy: Enemy,
  turn: number,
  log: ActionRest[],
  idGen: Generator,
): { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionRest[] } {
  // do BattleTurn action
  console.log("TEST " + turn);
  const battleTurn: BattleTurn = { tag: "BattleTurn", turn };
  const afterTurnResult = doAction(battleTurn, state, log, idGen);
  if (afterTurnResult.newState === "invalid") {
    return { result: "invalid", newLog: afterTurnResult.newLog };
  }

  state = afterTurnResult.newState;


  const meleeCrew: IdCrew | undefined = state.crew[0];
  const def = {
    result: {
      newState: state,
      newEnemy: enemy,
    },
    newLog: log,
  };
  const afterMelee = meleeCrew === undefined ? def : doAttack(meleeCrew, enemy, state, afterTurnResult.newLog, idGen);
  if (afterMelee.result === "invalid") {
    return afterMelee;
  }

  const afterRanged = afterMelee.result.newState.crew.slice(1).reduce((acc, crew) => {
    if (acc.result === "invalid") {
      return acc;
    }

    if (crew.ranged) {
      return doAttack(crew, acc.result.newEnemy, acc.result.newState, acc.newLog, idGen);
    } else {
      return acc;
    }
  }, afterMelee);

  if (afterRanged.result === "invalid") {
    return afterRanged;
  }
  let newEnemy = afterRanged.result.newEnemy;

  // use values of enemy before attack
  const actionIndex = turn % enemy.actions.length;
  const enemyRank = enemy.rank;
  const enemyAction: EnemyAttack | undefined = enemy.actions[actionIndex];

  if (enemyAction === undefined) {
    throw ("invalid actionIndex " + actionIndex);
  }

  let battleResult: { result: { newState: GameState, newEnemy: Enemy }, newLog: ActionRest[] }
    // prevent used before defined warning
    = (<any>"unused");
  switch (enemyAction.tag) {
    case "MeleeAttack": {
      const atkValue: number = enemyRank * enemyAction.multiplier;
      const action: Action<Target> = {
        tag: "Damage",
        positions: enemyAction.positions,
        value: atkValue,
      };
      const effectResult = doAction(action, afterRanged.result.newState, afterRanged.newLog, idGen);
      if (effectResult.newState === "invalid") {
        return { result: "invalid", newLog: effectResult.newLog };
      } else {
        battleResult = { result: { newState: effectResult.newState, newEnemy }, newLog: effectResult.newLog };
      }
      break;
    }
    case "Heal": {
      if (newEnemy.rank > 0) {
        newEnemy = focus(newEnemy, over(x => x.rank, x => x + enemyAction.value));
      }
      battleResult = { result: { newState: afterRanged.result.newState, newEnemy }, newLog: afterRanged.newLog };
      break;
    }
  }

  // handle dead crew
  // TODO: move this to after doAction?
  for (const ally of battleResult.result.newState.crew) {
    if (ally.hp <= 0) {
      const afterDeath = doAction({ tag: "Death", targetId: ally.id },
        battleResult.result.newState,
        battleResult.newLog,
        idGen
      );
      battleResult = focus(battleResult,
        set(x => x.result.newState, afterDeath.newState),
        set(x => x.newLog, afterDeath.newLog)
      );
    }
  }

  return battleResult;
}

export function runBattle(
  state: GameState,
  enemy: Enemy,
  log: ActionRest[],
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  const afterStartBattle = doAction({ tag: "StartBattle" }, state, log, idGen);
  if (afterStartBattle.newState === "invalid") {
    return afterStartBattle;
  }
  const afterBattle = _runBattle(afterStartBattle.newState, enemy, 0, afterStartBattle.newLog, idGen);
  if (afterBattle.newState === "invalid") {
    return afterBattle;
  }
  const afterClearTemp = focus(afterBattle,
    over(x => (<GameState>x.newState).crew, x => x.map(x => clearTemp(x)))
  );
  const afterEndBattle = doAction({ tag: "EndBattle" }, <GameState>afterClearTemp.newState, afterClearTemp.newLog, idGen);
  return afterEndBattle;
}

export function _runBattle(
  state: GameState,
  enemy: Enemy,
  turn: number,
  log: ActionRest[],
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  const { result, newLog } = battleStep(state, enemy, turn, log, idGen);
  if (result === "invalid") {
    return { newState: "invalid", newLog };
  } else if (result.newEnemy.rank <= 0) {
    return { newState: result.newState, newLog };
  } else {
    const { newState, newEnemy } = result;

    return _runBattle(newState, newEnemy, turn + 1, newLog, idGen);
  }
}

const enemyAtk012R10: Enemy = {
  rank: 10,
  actions: [
    {
      tag: "MeleeAttack",
      multiplier: 1,
      positions: [0, 1, 2],
    },
  ],
};

const enemyHeal2R14: Enemy = {
  rank: 14,
  actions: [
    {
      tag: "MeleeAttack",
      multiplier: 1,
      positions: [0],
    },
    {
      tag: "Heal",
      value: 2,
    },
  ],
};

export const allEnemies = {
  enemyAtk012: enemyAtk012R10,
  enemyHeal2R14: enemyHeal2R14,
};