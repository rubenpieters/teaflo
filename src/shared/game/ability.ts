import { focus, over, set } from "src/shared/iassign-util";
import { determineTarget } from "src/shared/game/target";
import { Action, ActionSpec, determineSpec, fmap } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { GameState } from "src/shared/game/state";

export type SwapSelf = {
  tag: "SwapSelf",
};

export type Ability
  = SwapSelf
  | ActionSpec
  ;

export function abilityToAction(
  ability: Ability,
  state: GameState,
  selfId: number,
): Action<Target> {
  switch (ability.tag) {
    case "SwapSelf": {
      return {
        tag: "Swap",
        type: "ally",
        from: selfId,
        to: 0, // TODO: from user input
      };
    }
    default: {
      const actionSpec = determineSpec(ability, state, selfId, "ally");
      const actionTarget = fmap(x => determineTarget(x, state, selfId, "ally", "noOrigin"), actionSpec);
      return actionTarget;
    }
  }
}