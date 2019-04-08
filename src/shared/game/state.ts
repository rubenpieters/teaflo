import { FrUnit, EnUnit } from "./unit";
import { HasId, TargetId, TargetType, EntityId, FriendlyId, EnemyId, StatusId, UnitType, friendlyId, enemyId } from "./entityId";
import { UnitRow } from "./unitRow";
import { StatusRow, StStatus } from "./statusRow";
import { focus, over, modifyAndGet } from "../iassign-util";
import { StatusGroup, groupOrder } from "./status";
import deepEqual = require("deep-equal");
import { frUnitMap, FrUnitId } from "../data/frUnitMap";
import { enUnitMap, EnUnitId } from "../data/enUnitMap";

export type StFrUnit = FrUnit & HasId<"friendly"> & { threatMap: ThreatMap };
export type StEnUnit = EnUnit & HasId<"enemy">;

export type HasThreatMap = {
  threatMap: ThreatMap,
}

export type ThreatMap = { [globalId: number]: number };

export function addThreat<U extends HasThreatMap>(
  u: U,
  atEnemy: EnemyId,
  value: number,
): U {
  return focus(u, over(x => x.threatMap[atEnemy.id], x => x === undefined ? value : x + value));
}

export type IdToEntityType = {
  "friendly": StFrUnit,
  "enemy": StEnUnit,
  "status": StStatus,
}

export type StateType
  = "invalid"
  | "win"
  | "default"
  ;

export class GameState {
  public readonly type: StateType = "default";
  public readonly statusRows: { [K in StatusGroup]: StatusRow };

  constructor(
    public readonly frUnits: UnitRow<"friendly", StFrUnit>,
    public readonly enUnits: UnitRow<"enemy", StEnUnit>,
    public readonly nextId: number = 0,
  ) {
    this.statusRows = {} as any;
    groupOrder.forEach(group => {
      this.statusRows[group] = new StatusRow();
    });
  }

  static make(
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
    return new GameState(
      new UnitRow("friendly", frUnitsWithId),
      new UnitRow("enemy", enUnitsWithId),
      enLastId + 1,
    );
  }

  frFiltered(): { e: StFrUnit, i: number }[] {
    return this.frUnits.defined();
  }

  frIds(): { e: FriendlyId, i: number }[] {
    return this.frFiltered().map(x => {
      return { e: x.e.id, i: x.i };
    });
  }

  enFiltered(): { e: StEnUnit, i: number }[] {
    return this.enUnits.defined();
  }

  enIds(): { e: EnemyId, i: number }[] {
    return this.enFiltered().map(x => {
      return { e: x.e.id, i: x.i };
    });
  }

  position<Type extends UnitType>(
    _target: EntityId<Type>,
  ): number | undefined {
    switch (_target.type) {
      case "friendly": {
        // compiler does not refine `Type`
        const target = _target as FriendlyId;

        const index = this.frUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
        if (index === -1) return undefined;
        return index;
      }
      case "enemy": {
        // compiler does not refine `Type`
        const target = _target as EnemyId;

        const index = this.enUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
        if (index === -1) return undefined;
        return index;
      }
      default: {
        throw "GameState.overTarget: impossible case";
      }
    }
  }

  statusPosition(
    statusId: StatusId,
  ): { rowPosition: number, columnPosition: number } | undefined {
    let rowPosition = 0;
    for (const group of groupOrder) {
      const columnPosition = this.statusRows[group].statusPosition(statusId);
      if (columnPosition !== undefined) {
        return { rowPosition, columnPosition };
      }
      rowPosition += 1;
    }
    return undefined;
  }

  overTarget<Type extends TargetType>(
    _target: EntityId<Type>,
    _f: (e: IdToEntityType[Type]) => IdToEntityType[Type],
  ): { state: GameState, entity?: IdToEntityType[Type] } {
    switch (_target.type) {
      case "friendly": {
        // compiler does not refine `Type`
        const target = _target as FriendlyId;
        const f = _f as (e: StFrUnit) => StFrUnit;

        const result = modifyAndGet(this,
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

        const result = modifyAndGet(this,
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
          const result = modifyAndGet(this,
            x => x.statusRows[group], x => {
              const result = x.overStatus(target, f);
              return { a: result.row, b: result.status };
            }
          );
          if (result.b !== undefined) {
            return { state: result.s, entity: result.b };
          }
        }

        return { state: this };
      }
      default: {
        throw "GameState.overTarget: impossible case";
      }
    }
  }

  getTarget<Type extends TargetType>(
    _target: EntityId<Type>,
  ): IdToEntityType[Type] | undefined {
    return this.overTarget(_target, x => x).entity;
  }

  removeTarget<Type extends TargetType>(
    _target: EntityId<Type>,
  ): { state: GameState, entity?: IdToEntityType[Type] } {
    switch (_target.type) {
      case "friendly": {
        // compiler does not refine `Type`
        const target = _target as FriendlyId;

        const result = modifyAndGet(this,
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

        const result = modifyAndGet(this,
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
          const result = modifyAndGet(this,
            x => x.statusRows[group], x => {
              const result = x.removeStatus(target);
              return { a: result.row, b: result.entity };
            }
          );
          if (result.b !== undefined) {
            return { state: result.s, entity: result.b };
          }
        }

        return { state: this };
      }
      default: {
        throw "GameState.overTarget: impossible case";
      }
    }
  }
}