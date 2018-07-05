import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";

type Id = {
  id: number,
};

export type IdCrew = Crew & Id;

export type GameState = {
  crew: IdCrew[],
};

export const initialState: GameState = {
  crew: [],
};