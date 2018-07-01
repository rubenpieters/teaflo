import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";
import { Enemy, runBattle } from "src/shared/game/enemy";

export type Recruit = {
  tag: "Recruit",
  crew: Crew,
}

export type Battle = {
  tag: "Battle",
  enemy: Enemy,
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
      const battleResult = runBattle(state.crew, action.enemy);
      if (battleResult === "invalid") {
        return "invalid"
      }
      return focus(state, set(x => x.crew, battleResult));
    }
    case "Recruit": {
      return focus(state,
        over(x => x.crew , x => [action.crew].concat(x))
      );
    }
  }
}