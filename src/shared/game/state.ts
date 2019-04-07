import { FrUnit, EnUnit } from "./unit";
import { HasId, TargetId, TargetType, EntityId, FriendlyId, EnemyId, StatusId } from "./entityId";
import { UnitRow } from "./unitRow";
import { StatusRow, StStatus } from "./statusRow";
import { focus, over, modifyAndGet } from "../iassign-util";
import { StatusGroup, groupOrder } from "./status";

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
  public readonly nextId: number = 0;
  public readonly type: StateType = "default";
  public readonly statusRows: { [K in StatusGroup]: StatusRow };

  constructor(
    public readonly frUnits: UnitRow<"friendly", StFrUnit>,
    public readonly enUnits: UnitRow<"enemy", StEnUnit>,
  ) {
    this.statusRows = {} as any;
    groupOrder.forEach(group => {
      this.statusRows[group] = new StatusRow();
    });
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