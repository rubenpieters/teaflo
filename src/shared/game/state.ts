import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";

type Id = {
  id: number,
};

export type IdCrew = Crew & Id;

export type GameState = {
  crew: IdCrew[],
  items: Item[],
  gold: number,
};

export const initialState: GameState = {
  crew: [],
  items: [],
  gold: 0,
};