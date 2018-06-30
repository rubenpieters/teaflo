import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";

export type Recruit = {
  tag: "Recruit",
  crew: Crew,
}

export type Battle = {
  tag: "Battle",
}

export type Rest = {
  tag: "Rest",
}

export type Action
  = Recruit
  | Battle

export function doAction(
  action: Action | Rest,
  state: GameState,
) {
  switch (action.tag) {
    case "Rest": {
      return state;
    }
    case "Battle": {
      return state;
    }
    case "Recruit": {
      return focus(state,
        over(x => x.crew , x => [action.crew].concat(x))
      );
    }
  }
}