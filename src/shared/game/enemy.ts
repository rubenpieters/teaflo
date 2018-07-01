import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Effect, applyEffect } from "src/shared/game/effect";

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
  crew: Crew[],
  enemy: Enemy,
  turn: number,
) {
  const fighter: Crew | undefined = crew[0];
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
      const effect: Effect = {
        tag: "Damage",
        positions: enemyAction.positions,
        value: atkValue,
      }
      const effectResult = applyEffect(effect, crew);
      if (effectResult === "invalid") {
        return "invalid";
      } else {
        return { newCrew: effectResult, newEnemy };
      }
    }
    case "Heal": {
      return { newCrew: crew, newEnemy };
    }
  }
}

export function runBattle(
  crew: Crew[],
  enemy: Enemy,
) {
  return _runBattle(crew, enemy, 0);
}

export function _runBattle(
  crew: Crew[],
  enemy: Enemy,
  turn: number,
): Crew[] | "invalid" {
  const battleResult = battleStep(crew, enemy, turn);
  if (battleResult === "invalid") {
    return "invalid"
  } else if (battleResult.newEnemy.rank < 0) {
    return battleResult.newCrew;
  } else {
    const { newCrew, newEnemy } = battleResult

    return _runBattle(newCrew, newEnemy, turn + 1);
  }
}