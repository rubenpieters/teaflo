import { focus, over, set } from "src/shared/iassign-util";
import { GameState, units, FrStUnit, EnStUnit } from "./state";
import { HasId } from "./hasId";
import { Unit } from "./unit";

export type TargetType
  = "friendly"
  | "enemy"
  ;

export class GlobalId<A extends TargetType>{
  constructor(
    public readonly id: number,
    public readonly type: A,
    public readonly tag: "GlobalId" = "GlobalId",
  ) {}
}

export class PositionId<A extends TargetType>{
  constructor(
    public readonly id: number,
    public readonly type: A,
    public readonly tag: "PositionId" = "PositionId",
  ) {}
}

export function isGlobalId<A extends TargetType>(
  a: any
): a is GlobalId<A> {
  return a.tag === "GlobalId";
}

export function isPositionId<A extends TargetType>(
  a: any
): a is PositionId<A> {
  return a.tag === "PositionId";
}

export type EntityId<A extends TargetType>
  = GlobalId<A>
  | PositionId<A>
  ;

export type UnitId = EntityId<"friendly" | "enemy">;

export function killUnit(
  target: UnitId,
  state: GameState,
) {
  const id = findIndex(state, target);
  switch (target.type) {
    case "friendly": {
      return focus(state,
        set(x => x.frUnits[id], undefined),
      );
    }
    case "enemy": {
      return focus(state,
        set(x => x.enUnits[id], undefined),
      );
    }
  }
}

export function getUnit(
  target: UnitId,
  state: GameState,
) {
  const id = findIndex(state, target);
  switch (target.type) {
    case "friendly": {
      return state.frUnits[id];
    }
    case "enemy": {
      return state.enUnits[id];
    }
  }
}

export function overUnit(
  target: UnitId,
  state: GameState,
  f: (unit: Unit) => Unit,
  onEmpty: (state: GameState) => GameState,
): GameState {
  const id = findIndex(state, target);
  switch (target.type) {
    case "friendly": {
      if (state.frUnits[id] === undefined) {
        return onEmpty(state);
      } else {
        // state.frUnits[id] is checked before
        return focus(state,
          over(x => x.frUnits[id], <any>f),
        );
      }
    }
    case "enemy": {
      if (state.enUnits[id] === undefined) {
        return onEmpty(state);
      } else {
        // state.enUnits[id] is checked before
        return focus(state,
          over(x => x.enUnits[id], <any>f),
        );
      }
    }
  }
}

// TODO: only accept EntityId<"friendly">
export function overFriendly(
  target: UnitId,
  state: GameState,
  f: (unit: FrStUnit) => FrStUnit,
  onEmpty: (state: GameState) => GameState,
) {
  if (target.type !== "friendly") {
    throw `overFriendly: wrong target type ${JSON.stringify(target)}`;
  }
  const id = findIndex(state, target);
  if (state.frUnits[id] === undefined) {
    return onEmpty(state);
  } else {
    // state.frUnits[id] is checked before
    return focus(state,
      over(x => x.frUnits[id], <any>f),
    );
  }
}

// TODO: only accept EntityId<"enemy">
export function overEnemy(
  target: UnitId,
  state: GameState,
  f: (unit: EnStUnit) => EnStUnit,
  onEmpty: (state: GameState) => GameState,
) {
  if (target.type !== "enemy") {
    throw `overEnemy: wrong target type ${JSON.stringify(target)}`;
  }
  const id = findIndex(state, target);
  if (state.enUnits[id] === undefined) {
    return onEmpty(state);
  } else {
    // state.enUnits[id] is checked before
    return focus(state,
      over(x => x.enUnits[id], <any>f),
    );
  }
}

export function toPositionId<A extends TargetType>(
  state: GameState,
  id: EntityId<A>,
) {
  switch (id.tag) {
    case "PositionId": return id;
    case "GlobalId": {
      return new PositionId(findIndex(state, id), id.type);
    }
  }
}

export function toGlobalId<A extends TargetType>(
  state: GameState,
  id: EntityId<A>,
) {
  switch (id.tag) {
    case "PositionId": {
      const unit = units(state, id.type)[id.id];
      if (unit === undefined) {
        throw `unit at position ${JSON.stringify(id)} is undefined`;
      }
      return new GlobalId(unit.id, id.type);
    };
    case "GlobalId": return id;
  }
}

export function findIndex<A extends TargetType>(
  state: GameState,
  id: EntityId<A>,
): number {
  switch (id.type) {
    case "friendly": {
      return findI(state.frUnits, id);
    }
    case "enemy": {
      return findI(state.enUnits, id);
    }
  }
  // ts compiler does not find exhaustiveness for A
  throw `findIndex: unknown type ${id.type}`;
}

function findI<E extends HasId, A extends TargetType>(
  coll: (E | undefined)[],
  id: EntityId<A>,
): number {
  switch (id.tag) {
    case "PositionId": {
      return id.id;
    }
    case "GlobalId": {
      const e: number = coll.findIndex(x => x !== undefined && x.id === id.id);
      if (e === -1) {
        throw `findI: not found ${JSON.stringify(id)}`;
      }
      return e;
    }
  }
}

export function eqUnitId<A extends TargetType>(
  state: GameState,
  id1: EntityId<A>,
  id2: EntityId<A>,
): boolean {
  if (id1.tag === "PositionId" && id2.tag === "PositionId") {
    return id1.type === id2.type && id1.id === id2.id;
  } else if (id1.tag === "GlobalId" && id2.tag === "GlobalId") {
    return id1.type === id2.type && id1.id === id2.id;
  } else {
    const positionId1 = toPositionId(state, id1);
    const positionId2 = toPositionId(state, id2);
    return eqUnitId(state, positionId1, positionId2);
  }
}

function targetTypeToString<A extends TargetType>(
  type: A,
) {
  switch (type) {
    case "enemy": return "EN";
    case "friendly": return "FR";
  }
  throw "targetTypeToString: impossible";
}

export function posToString<A extends TargetType>(
  id: EntityId<A>,
) {
  switch (id.tag) {
    case "GlobalId": return `GID ${id.id} ${targetTypeToString(id.type)}`;
    case "PositionId": return `PID ${id.id} ${targetTypeToString(id.type)}`;
  }
}