import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { Target } from "src/shared/game/target";

export type SwapSelf = {
  tag: "SwapSelf",
};

export type HealSelf = {
  tag: "HealSelf",
}

export type Ability
  = SwapSelf
  | HealSelf

export function abilityToAction(
  ability: Ability,
  selfId: number,
): Action<Target> {
  switch (ability.tag) {
    case "SwapSelf": {
      return {
        tag: "Swap",
        type: "ally",
        from: selfId,
        to: 0, // TODO: from user input
      }
    }
    case "HealSelf": {
      throw "unimplemented";
    }
  }
}