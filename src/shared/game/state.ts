import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";
import { Enemy } from "src/shared/game/enemy";

export type Id = {
  id: number,
};

export type ActionIndex = {
  actionIndex: number,
};

export type IdCrew = Crew & Id & ActionIndex;
export type IdItem = Item & Id;
export type IdEnemy = Enemy & Id & ActionIndex;

export type GameState = {
  crew: IdCrew[],
  enemies: IdEnemy[],
  items: IdItem[],
  gold: number,
};

export const initialState: GameState = {
  crew: [],
  enemies: [],
  items: [],
  gold: 0,
};