import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { Item } from "src/shared/game/item";
import { Enemy } from "src/shared/game/enemy";
import { Origin, TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { HasStatus } from "src/shared/game/status";
import { Instance } from "./instance";

export type Id = {
  id: number,
};

export type ActionIndex = {
  actionIndex: number,
};

export type IdCrew = Crew & Id & ActionIndex & { tag: "ally" };
export type IdItem = Item & Id & { tag: "item" };
export type IdEnemy = Enemy & Id & ActionIndex & { tag: "enemy" };
export type IdInstance = Instance & Id;

export type GameState = {
  crew: IdCrew[],
  allyInstances: IdInstance[],
  enemies: IdEnemy[],
  enemyInstances: IdInstance[],
  items: IdItem[],
  gold: number,
  crewLimit: number,
  actionQueue: { action: Action, origin: Origin }[],
};

export const initialState: GameState = {
  crew: [],
  allyInstances: [],
  enemies: [],
  enemyInstances: [],
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

export function showId<A extends TargetType>(id: EntityIdA<A>): string {
  switch (id.tag) {
    case "PositionId": {
      return `Pos [${id.id}] ${id.type}`;
    }
    case "GlobalId": {
      return `GID [${id.id}] ${id.type}`;
    }
  }
}

export type EntityId = EntityIdA<TargetType>;

export type CreatureId = EntityIdA<"ally" | "enemy">

type TargetEntity = {
  "ally": IdCrew,
  "enemy": IdEnemy,
  "item": IdItem,
  "allyInstance": IdInstance,
  "enemyInstance": IdInstance,
}

export function findEntity<A extends TargetType>(state: GameState, id: EntityIdA<A>): TargetEntity[A] {
  const type: TargetType = id.type;
  switch (type) {
    case "ally": {
      return findE(state.crew, id);
    }
    case "enemy": {
      return findE(state.enemies, id);
    }
    case "item": {
      return findE(state.items, id);
    }
    case "allyInstance": {
      return findE(state.allyInstances, id);
    }
    case "enemyInstance": {
      return findE(state.enemyInstances, id);
    }
  }
}

function findE<E extends Id>(coll: E[], id: EntityId): E {
  switch (id.tag) {
    case "PositionId": {
      return coll[id.id];
    }
    case "GlobalId": {
      const e: E | undefined = coll.find(x => x.id === id.id);
      if (e === undefined) {
        throw `findE: not found ${JSON.stringify(id)}`;
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

export function toGlobalId<A extends TargetType>(state: GameState, id: EntityIdA<A>): GlobalIdA<A> {
  switch (id.tag) {
    case "PositionId": {
      return {
        tag: "GlobalId",
        // can be more efficient since we know that we have a position id
        id: findEntity(state, id).id,
        type: id.type,
      }
    }
    case "GlobalId": {
      return id;
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
  throw `findIndex: unknown type ${id.type}`;
}

function findI<E extends Id>(coll: E[], id: EntityId): number {
  switch (id.tag) {
    case "PositionId": {
      return id.id;
    }
    case "GlobalId": {
      const e: number = coll.findIndex(x => x.id === id.id);
      if (e === -1) {
        throw `findI: not found ${JSON.stringify(id)}`;
      }
      return e;
    }
  }
}

export function inCombat(state: GameState): boolean {
  return state.enemies.length > 0;
}

export function onCreature(
  target: CreatureId,
  state: GameState,
  allyF: (ally: IdCrew) => IdCrew,
  enemyF: (enemy: IdEnemy) => IdEnemy,
) {
  const position = toPositionId(state, target);
  switch (target.type) {
    case "ally": {
      return focus(state,
        over(x => x.crew[position.id], allyF),
      );
    }
    case "enemy": {
      return focus(state,
        over(x => x.enemies[position.id], enemyF),
      );
    }
  }
}

export function entityExists<A extends TargetType>(
  id: EntityIdA<A>,
  state: GameState,
) {
  switch (id.type) {
    case "ally": {
      return exists(state.crew, id);
    }
    case "enemy": {
      return exists(state.enemies, id);
    }
    case "item": {
      return exists(state.items, id);
    }
  }
  // ts compiler does not find exhaustiveness for A
  throw `entityExists: unknown type ${id.type}`;
}

export function exists<A extends TargetType, E extends Id>(
  coll: E[],
  id: EntityIdA<A>,
) {
  switch (id.tag) {
    case "PositionId": {
      if (id.id >= coll.length) {
        return false;
      }
      return true;
    }
    case "GlobalId": {
      const e: E | undefined = coll.find(x => x.id === id.id);
      if (e === undefined) {
        return false;
      }
      return true;
    }
  }
}

export function idEqual<A extends TargetType>(
  state: GameState,
  id1: EntityIdA<A>,
  id2: EntityIdA<A>,
): boolean {
  if (id1.tag === "PositionId" && id2.tag === "PositionId") {
    return id1.type === id2.type && id1.id === id2.id;
  } else if (id1.tag === "GlobalId" && id2.tag === "GlobalId") {
    return id1.type === id2.type && id1.id === id2.id;
  } else {
    const positionId1 = toPositionId(state, id1);
    const positionId2 = toPositionId(state, id2);
    return idEqual(state, positionId1, positionId2);
  }
}