import { TargetType, EntityId, FriendlyId, EnemyId, StatusId, UnitType, friendlyId, enemyId } from "../definitions/entityId";
import { UnitRow } from "../definitions/unitRow";
import { StatusRow, StStatus } from "../definitions/statusRow";
import { focus, over, modifyAndGet } from "../iassign-util";
import { groupOrder } from "./status";
import deepEqual from "deep-equal";
import { frUnitMap, FrUnitId } from "../data/frUnitMap";
import { enUnitMap, EnUnitId } from "../data/enUnitMap";
import { StFrUnit, StEnUnit, GameState } from "../definitions/state";

export type IdToEntityType = {
  "friendly": StFrUnit,
  "enemy": StEnUnit,
  "status": StStatus,
}

export function mkGameState(
  frUnits: (FrUnitId | undefined)[],
  enUnits: (EnUnitId | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (StFrUnit | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId += 1;
      return {...frUnitMap[x], id: friendlyId(i), cardId: x, threatMap: {}, };
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (StEnUnit | undefined)[] = enUnits.map((x, i) => {
    if (x !== undefined) {
      enLastId += 1;
      return {...enUnitMap[x], id: enemyId(i + frLastId), };
    } else {
      return undefined;
    }
  });
  const statusRows = {} as any;
  groupOrder.forEach(group => {
    statusRows[group] = new StatusRow();
  });
  return new GameState(
    new UnitRow("friendly", frUnitsWithId),
    new UnitRow("enemy", enUnitsWithId),
    enLastId + 1,
    statusRows
  );
}

export function frFiltered(
  state: GameState
): { e: StFrUnit, i: number }[] {
  return state.frUnits.defined();
}

export function frIds(
  state: GameState,
): { e: FriendlyId, i: number }[] {
  return frFiltered(state).map(x => {
    return { e: x.e.id, i: x.i };
  });
}

export function enFiltered(
  state: GameState,
): { e: StEnUnit, i: number }[] {
  return state.enUnits.defined();
}

export function enIds(
  state: GameState,
): { e: EnemyId, i: number }[] {
  return enFiltered(state).map(x => {
    return { e: x.e.id, i: x.i };
  });
}

export function position<Type extends UnitType>(
  state: GameState,
  _target: EntityId<Type>,
): number | undefined {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;

      const index = state.frUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
      if (index === -1) return undefined;
      return index;
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;

      const index = state.enUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
      if (index === -1) return undefined;
      return index;
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}

export function statusPosition(
  state: GameState,
  statusId: StatusId,
): { rowPosition: number, columnPosition: number } | undefined {
  let rowPosition = 0;
  for (const group of groupOrder) {
    const columnPosition = state.statusRows[group].statusPosition(statusId);
    if (columnPosition !== undefined) {
      return { rowPosition, columnPosition };
    }
    rowPosition += 1;
  }
  return undefined;
}

export function overTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
  _f: (e: IdToEntityType[Type]) => IdToEntityType[Type],
): { state: GameState, entity?: IdToEntityType[Type] } {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;
      const f = _f as (e: StFrUnit) => StFrUnit;

      const result = modifyAndGet(state,
        x => x.frUnits, x => {
          const result = x.overUnit(target, f);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;
      const f = _f as (e: StEnUnit) => StEnUnit;

      const result = modifyAndGet(state,
        x => x.enUnits, x => {
          const result = x.overUnit(target, f);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "status": {
      // compiler does not refine `Type`
      const target = _target as StatusId;
      const f = _f as (e: StStatus) => StStatus;

      for (const group of groupOrder) {
        const result = modifyAndGet(state,
          x => x.statusRows[group], x => {
            const result = x.overStatus(target, f);
            return { a: result.row, b: result.status };
          }
        );
        if (result.b !== undefined) {
          return { state: result.s, entity: result.b };
        }
      }

      return { state };
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}

export function getTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
): IdToEntityType[Type] | undefined {
  return overTarget(state, _target, x => x).entity;
}

export function removeTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
): { state: GameState, entity?: IdToEntityType[Type] } {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;

      const result = modifyAndGet(state,
        x => x.frUnits, x => {
          const result = x.removeUnit(target);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;

      const result = modifyAndGet(state,
        x => x.enUnits, x => {
          const result = x.removeUnit(target);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "status": {
      // compiler does not refine `Type`
      const target = _target as StatusId;

      for (const group of groupOrder) {
        const result = modifyAndGet(state,
          x => x.statusRows[group], x => {
            const result = x.removeStatus(target);
            return { a: result.row, b: result.entity };
          }
        );
        if (result.b !== undefined) {
          return { state: result.s, entity: result.b };
        }
      }

      return { state };
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}
