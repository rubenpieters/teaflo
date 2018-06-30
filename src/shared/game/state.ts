import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";

export type GameState = {
  crew: Crew[],
}

export const initialState: GameState = {
  crew: [],
}