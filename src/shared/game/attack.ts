import { focus, over, set } from "src/shared/iassign-util";
import { Enemy } from "src/shared/game/enemy";
import { Action, ActionTarget, doAction, fmap } from "src/shared/game/action";
import { GameState, IdCrew } from "src/shared/game/state";
import { TargetSpec, findTarget } from "src/shared/game/target";
import { Crew, damage, getAP } from "src/shared/game/crew";
import { Generator } from "src/shared/handler/id/generator";

export type Damage = {
  tag: "Damage",
  multiplier: number,
};

export type Status = {
  tag: "Status"
};

export type ActionAttack = {
  tag: "ActionAttack",
  action: Action<TargetSpec>,
};

export type Attack
  = Damage
  | Status
  | ActionAttack
  ;

export function doAttack(
 crew: IdCrew,
 enemy: Enemy,
 state: GameState,
 log: ActionTarget[],
 idGen: Generator,
): { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionTarget[] } {
  let acc: { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionTarget[] } = {
    result: {
      newState: state,
      newEnemy: enemy,
    },
    newLog: log
  };
  for (const attack of crew.attack) {
    if (acc.result === "invalid") {
      return acc;
    }

    switch (attack.tag) {
      case "Damage": {
        acc = {
          result: {
            newState: state,
            newEnemy: focus(enemy, over(x => x.rank, x => x - getAP(crew, attack.multiplier))),
          },
          newLog: log
        };
        break;
      }
      case "Status": {
        break;
      }
      case "ActionAttack": {
        const action = fmap(x => findTarget(x, state, crew.id, "ally"), attack.action);
        const result = doAction(action, acc.result.newState, acc.newLog, idGen);
        if (result.newState === "invalid") {
          acc = {
            result: "invalid",
            newLog: result.newLog,
          };
        } else {
          acc = {
            result: {
              newState: result.newState,
              newEnemy: enemy,
            },
            newLog: result.newLog,
          };
        }
        break;
      }
    }
  }
  return acc;
}