import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";
import { Enemy } from "src/shared/game/enemy";
import { HasStatus } from "src/shared/game/status";

export type Id = {
  id: number,
};

export type ActionIndex = {
  actionIndex: number,
};

export type IdCrew = Crew & Id & ActionIndex & HasStatus;
export type IdItem = Item & Id;
export type IdEnemy = Enemy & Id & ActionIndex & HasStatus;

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