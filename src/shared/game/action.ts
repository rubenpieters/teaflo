import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";
import { Enemy, runBattle } from "src/shared/game/enemy";
import { LoggedEffect } from "./log";

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

export type Damage = {
  tag: "Damage",
  positions: number[],
  value: number,
}

export type BattleTurn = {
  tag: "BattleTurn",
  turn: number,
}

export type Action
  = Recruit
  | Battle
  | Damage
  | BattleTurn

export function doAction(
  action: Action | Rest,
  state: GameState,
): { newState: GameState | "invalid", log: (Action | Rest)[] } {
  /* crew interactions with effects
  for (const ally of crew) {

  }
  */

  switch (action.tag) {
    case "Rest": {
      return { newState: state, log: [action] };
    }
    case "Damage": {
      let resultCrew: Crew[] = state.crew;
      for (const position in action.positions) {
        const allyAtPos: Crew | undefined = state.crew[position];
        if (allyAtPos === undefined) {
          return { newState: "invalid", log: [action] }
        }
        resultCrew = focus(resultCrew,
          over(x => x[position].hp, x => x - action.value)
        )
      }
      const newState = focus(state, set(x => x.crew, resultCrew));
      return { newState: state, log: [action] };
    }
    case "Battle": {
      const { newState, log } = runBattle(state, action.enemy);
      const l: (Action | Rest)[] = [action];
      if (newState === "invalid") {
        return { newState: "invalid", log: l.concat(log) }
      }
      return { newState , log: l.concat(log) };
    }
    case "Recruit": {
      const newState = focus(state,
        over(x => x.crew, x => [action.crew].concat(x))
      );
      return { newState, log: [action] };
    }
    case "BattleTurn": {
      return { newState: state, log: [action] };
    }
  }
}