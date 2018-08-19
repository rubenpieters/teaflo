import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";
import { Enemy } from "src/shared/game/enemy";
import { Origin } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { HasStatus } from "src/shared/game/status";

export type Id = {
  id: number,
};

export type ActionIndex = {
  actionIndex: number,
};

export type IdCrew = Crew & Id & ActionIndex & HasStatus & { tag: "ally" };
export type IdItem = Item & Id & { tag: "item" };
export type IdEnemy = Enemy & Id & ActionIndex & HasStatus & { tag: "enemy" };

export type GameState = {
  crew: IdCrew[],
  enemies: IdEnemy[],
  items: IdItem[],
  gold: number,
  crewLimit: number,
  actionQueue: { action: Action, origin: Origin }[],
};

export const initialState: GameState = {
  crew: [],
  enemies: [],
  items: [],
  gold: 0,
  crewLimit: 4,
  actionQueue: [],
};