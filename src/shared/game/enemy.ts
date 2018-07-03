import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";
import { Action, Rest, BattleTurn, doAction } from "src/shared/game/action";

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
): { result: { newState: GameState, newEnemy: Enemy } | "invalid", log: (Action | Rest)[] } {
  // do BattleTurn action
  const battleTurn: BattleTurn = { tag: "BattleTurn", turn };
  const afterTurnResult = doAction(battleTurn, state);
  if (afterTurnResult.newState === "invalid") {
    return { result: "invalid", log: [battleTurn] };
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
      const action: Action = {
        tag: "Damage",
        positions: enemyAction.positions,
        value: atkValue,
      }
      const effectResult = doAction(action, state);
      if (effectResult.newState === "invalid") {
        return { result: "invalid", log: [battleTurn, action] };
      } else {
        return { result: { newState: effectResult.newState, newEnemy }, log: [battleTurn, action] };
      }
    }
    case "Heal": {
      return { result: { newState: state, newEnemy }, log: [] };
    }
  }
}

export function runBattle(
  state: GameState,
  enemy: Enemy,
) {
  return _runBattle(state, enemy, 0);
}

export function _runBattle(
  state: GameState,
  enemy: Enemy,
  turn: number,
): { newState: GameState | "invalid", log: (Action | Rest)[] } {
  const { result, log } = battleStep(state, enemy, turn);
  if (result === "invalid") {
    return { newState: "invalid", log }
  } else if (result.newEnemy.rank < 0) {
    return { newState: result.newState, log };
  } else {
    const { newState, newEnemy } = result

    return _runBattle(newState, newEnemy, turn + 1);
  }
}