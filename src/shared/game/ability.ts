import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { GameState } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";


export function createCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): Card {
  return {
    id: "created",
    name: "-- created --",
    actions: [
      { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
        return action;
        }},
        inputs: [],
      },
    ],
    tag: "event",
    subtag: "general",
    origin: {
      id: selfId,
      type: selfType,
    }
  };
}