import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";
import { Enemy } from "src/shared/game/enemy";
import { Origin, TargetType, typeColl } from "src/shared/game/target";
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

export type GlobalIdA<A> = {
  tag: "GlobalId",
  id: number,
  type: A,
};

export type PositionIdA<A> = {
  tag: "PositionId",
  id: number,
  type: A,
};

type EntityIdA<A>
  = GlobalIdA<A>
  | PositionIdA<A>
  ;


export type EntityId = EntityIdA<TargetType>;

export type CreatureId = EntityIdA<"ally" | "enemy">

type TargetEntity<A extends TargetType> =
  A extends "ally" ? IdCrew :
  A extends "enemy" ? IdEnemy :
  A extends "item" ? IdItem :
  never
  ;

export function findEntity<A extends TargetType>(state: GameState, id: EntityIdA<A>): TargetEntity<A> {
  switch (id.type) {
    case "ally": {
      return <TargetEntity<A>>(findE(state.crew, id));
    }
    case "enemy": {
      return <TargetEntity<A>>(findE(state.enemies, id));
    }
    case "item": {
      return <TargetEntity<A>>(findE(state.items, id));
    }
  }
  // ts compiler does not find exhaustiveness for A
  return <any>undefined;
}

function findE<E extends Id>(coll: E[], id: EntityId): E {
  switch (id.tag) {
    case "PositionId": {
      return coll[id.id];
    }
    case "GlobalId": {
      const e: E | undefined = coll.find(x => x.id === id.id);
      if (e === undefined) {
        throw `not found ${id}`;
      }
      return e;
    }
  }
}

export function toPositionId<A extends TargetType>(state: GameState, id: EntityIdA<A>): PositionIdA<A> {
  switch (id.tag) {
    case "PositionId": {
      return id;
    }
    case "GlobalId": {
      return {
        tag: "PositionId",
        id: findIndex(state, id),
        type: id.type,
      }
    }
  }
}

export function findIndex<A extends TargetType>(state: GameState, id: EntityIdA<A>): number {
  switch (id.type) {
    case "ally": {
      return findI(state.crew, id);
    }
    case "enemy": {
      return findI(state.enemies, id);
    }
    case "item": {
      return findI(state.items, id);
    }
  }
  // ts compiler does not find exhaustiveness for A
  return <any>undefined;
}

function findI<E extends Id>(coll: E[], id: EntityId): number {
  switch (id.tag) {
    case "PositionId": {
      return id.id;
    }
    case "GlobalId": {
      const e: number = coll.findIndex(x => x.id === id.id);
      if (e === -1) {
        throw `not found ${id}`;
      }
      return e;
    }
  }
}

export function inCombat(state: GameState): boolean {
  return state.enemies.length > 0;
}