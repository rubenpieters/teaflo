import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";
import { ActionRest, Action, BattleTurn, doAction } from "src/shared/game/action";
import { Target } from "src/shared/game/target";

export type Enemy = {
  rank: number,
  actions: EnemyAttack[],
}

type MeleeAttack = {
  tag: "MeleeAttack",
  multiplier: number,
  positions: number[],
}

type Heal = {
  tag: "Heal",
}

export type EnemyAttack
  = MeleeAttack
  | Heal

function battleStep(
  state: GameState,
  enemy: Enemy,
  turn: number,
  log: ActionRest[],
): { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionRest[] } {
  // do BattleTurn action
  const battleTurn: BattleTurn = { tag: "BattleTurn", turn };
  const afterTurnResult = doAction(battleTurn, state, log, 0);
  if (afterTurnResult.newState === "invalid") {
    return { result: "invalid", newLog: afterTurnResult.newLog };
  }

  state = afterTurnResult.newState;

  const fighter: Crew | undefined = state.crew[0];
  const fighterAtk = fighter === undefined ? 0 : fighter.ap;
  const newEnemy: Enemy = focus(enemy, over(x => x.rank, x => x - fighterAtk));

  const actionIndex = turn % enemy.actions.length;
  const enemyAction: EnemyAttack | undefined = enemy.actions[actionIndex]

  if (enemyAction === undefined) {
    throw ("invalid actionIndex " + actionIndex);
  }

  switch (enemyAction.tag) {
    case "MeleeAttack": {
      const atkValue: number = enemy.rank * enemyAction.multiplier;
      const action: Action<Target> = {
        tag: "Damage",
        positions: enemyAction.positions,
        value: atkValue,
      }
      const effectResult = doAction(action, state, afterTurnResult.newLog, 0);
      if (effectResult.newState === "invalid") {
        return { result: "invalid", newLog: effectResult.newLog };
      } else {
        return { result: { newState: effectResult.newState, newEnemy }, newLog: effectResult.newLog };
      }
    }
    case "Heal": {
      return { result: { newState: state, newEnemy }, newLog: log };
    }
  }
}

export function runBattle(
  state: GameState,
  enemy: Enemy,
  log: ActionRest[],
) {
  return _runBattle(state, enemy, 0, log);
}

export function _runBattle(
  state: GameState,
  enemy: Enemy,
  turn: number,
  log: ActionRest[],
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  const { result, newLog } = battleStep(state, enemy, turn, log);
  if (result === "invalid") {
    return { newState: "invalid", newLog }
  } else if (result.newEnemy.rank < 0) {
    return { newState: result.newState, newLog };
  } else {
    const { newState, newEnemy } = result

    return _runBattle(newState, newEnemy, turn + 1, newLog);
  }
}