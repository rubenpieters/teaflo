import { focus, over, set } from "src/shared/iassign-util";
import { Enemy } from "src/shared/game/enemy";
import { Action, ActionRest, doAction, fmap } from "src/shared/game/action";
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
}

export type Attack
  = Damage
  | Status
  | ActionAttack
  ;

export function doAttack(
 crew: IdCrew,
 enemy: Enemy,
 state: GameState,
 log: ActionRest[],
 idGen: Generator,
): { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionRest[] } {
  let acc: { result: { newState: GameState, newEnemy: Enemy } | "invalid", newLog: ActionRest[] } = {
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
        const action = fmap(x => findTarget(x, crew.id), attack.action);
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