import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdEnemy } from "src/shared/game/state";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { Guard, HasStatus, Status, Transform, TransformTag, StatusTag } from "src/shared/game/status";
import { EnemyEffect, TriggerEntityEffect, EntityEffect } from "src/shared/game/ability";
import { Eff1 } from "./effectvar";
import { Next } from "./next";
import { ApplyActionLog } from "./log";

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
  charges: number,
  fragmentLoss: { [key in StatusTag | TransformTag]?: number },
  status: Status[],
  transforms: Transform[],
};

export function damage<E extends Enemy & HasStatus>(
  enemy: E,
  damage: number,
  piercing: boolean,
): E {
  return focus(enemy, over(x => x.hp, x => x - damage));
  /*if (piercing || (enemy.status.Guard === undefined && enemy.status.Bubble === undefined)) {
    return focus(enemy, over(x => x.hp, x => x - damage));
  } else {
    if (enemy.status.Bubble !== undefined) {
      return focus(enemy,
        set(x => x.status.Bubble, undefined),
      );
    } else { // crew.Guard !== undefined
      if (damage <= enemy.status.Guard!.guard) {
        return focus(enemy, over(x => (<Guard>x.status.Guard).guard, x => x - damage));
      }
  
      const leftoverDamage = damage - enemy.status.Guard!.guard;
      return focus(enemy,
        set(x => x.status.Guard, undefined),
        over(x => x.hp, x => x - leftoverDamage),
      );
    }
  }*/
}

export function act(
  enemy: IdEnemy,
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: ApplyActionLog }  {
  const { action, next } = enemy.actions[enemy.actionIndex].effect({ state, selfId: { tag: "GlobalId", id: enemy.id, type: "enemy" }});
  state = focus(state,
    over(x => x.enemies[index].actionIndex, x => nextActionId(state, enemy, x, next)),
  );
  return applyActionAndTriggers(
    action, state, log, idGen, { tag: "GlobalId", id: enemy.id, type: "enemy" }
  );
}

function nextActionId(
  state: GameState,
  enemy: Enemy,
  x: number,
  next: Next,
): number {
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
    case "NextCondition": {
      if (next.condition(state)) {
        return nextActionId(state, enemy, x, next.ifT);
      } else {
        return nextActionId(state, enemy, x, next.ifF);
      }
    }
  }
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