import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdEnemy } from "src/shared/game/state";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { Guard, HasStatus } from "src/shared/game/status";
import { EnemyEffect, TriggerEntityEffect } from "src/shared/game/ability";

/*export function showEnemy(
  enemy: Enemy
) {
  return {...enemy,
    actions: enemy.actions.map(showAction),
    triggers: enemy.triggers.map(showTrigger),
  };
}*/

export type Enemy = {
  hp: number,
  ap: number,
  maxHp: number,
  actions: EnemyEffect[],
  triggers: TriggerEntityEffect[],
};

export function damage<E extends Enemy & HasStatus>(
  enemy: E,
  damage: number,
  piercing: boolean,
): E {
  if (piercing || (enemy.Guard === undefined && enemy.Bubble === undefined)) {
    return focus(enemy, over(x => x.hp, x => x - damage));
  } else {
    if (enemy.Bubble !== undefined) {
      return focus(enemy,
        set(x => x.Bubble, undefined),
      );
    } else { // crew.Guard !== undefined
      if (damage <= enemy.Guard!.guard) {
        return focus(enemy, over(x => (<Guard>x.Guard).guard, x => x - damage));
      }
  
      const leftoverDamage = damage - enemy.Guard!.guard;
      return focus(enemy,
        set(x => x.Guard, undefined),
        over(x => x.hp, x => x - leftoverDamage),
      );
    }
  }
}

export function act(
  enemy: IdEnemy,
  state: GameState,
  log: Action[],
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: Action[] }  {
  const { action, next } = enemy.actions[enemy.actionIndex].effect(state, { tag: "GlobalId", id: enemy.id, type: "enemy" });
  state = focus(state,
    over(x => x.enemies[index].actionIndex, x => {
      switch (next.tag) {
        case "NextId": {
          const newX = x + 1;
          return newX >= enemy.actions.length ? 0 : newX;
        }
        case "Repeat": {
          return x;
        }
        case "Goto": {
          return next.action;
        }
      }
    }),
  );
  return applyActionAndTriggers(action, state, log, idGen, { id: enemy.id, type: "enemy" });
}

export function heal<E extends Enemy>(
  e: E,
  amount: number,
) {
  if (e.hp + amount > e.maxHp) {
    return focus(e, set(x => x.hp, e.maxHp));
  }
  return focus(e, set(x => x.hp, e.hp + amount));
}