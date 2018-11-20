import { focus, over, set } from "src/shared/iassign-util";
import { GameState } from "./state";
import { HasId } from "./hasId";
import { Unit } from "./unit";

export type TargetType
  = "friendly"
  | "enemy"
  ;

type GlobalId<A> = {
  tag: "GlobalId",
  id: number,
  type: A,
}

type PositionId<A> = {
  tag: "PositionId",
  id: number,
  type: A,
}

export type EntityId<A>
  = GlobalId<A>
  | PositionId<A>
  ;

export type UnitId = EntityId<"friendly" | "enemy">;

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

export function toPositionId<A extends TargetType>(
  state: GameState,
  id: EntityId<A>,
) {
  switch (id.tag) {
    case "PositionId": return id;
    case "GlobalId": {
      return {
        tag: "PositionId",
        id: findIndex(state, id),
        type: id.type,
      }
    }
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

function findI<E extends HasId, A>(
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