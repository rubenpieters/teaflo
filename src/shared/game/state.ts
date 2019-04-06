import { FrUnit, EnUnit } from "./unit";
import { HasId, TargetId, TargetType, EntityId, FriendlyId, EnemyId, StatusId } from "./entityId";
import { UnitRow } from "./unitRow";
import { StatusRow, StStatus } from "./statusRow";
import { focus, over, modifyAndGet } from "../iassign-util";

export type StFrUnit = FrUnit & HasId & { threatMap: ThreatMap };
export type StEnUnit = EnUnit & HasId;

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

export class GameState {
  constructor(
    public readonly frUnits: UnitRow<"friendly", StFrUnit>,
    public readonly enUnits: UnitRow<"enemy", StEnUnit>,
    public readonly statusRow: StatusRow,
  ) {}

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
        const result = modifyAndGet(this,
          x => x.statusRow, x => {
            const result = x.overStatus(target, f);
            return { a: result.row, b: result.status };
          }
        );

        return { state: result.s, entity: result.b };
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

        const result = modifyAndGet(this,
          x => x.statusRow, x => {
            const result = x.removeStatus(target);
            return { a: result.row, b: result.entity };
          }
        );

        return { state: result.s, entity: result.b };
      }
      default: {
        throw "GameState.overTarget: impossible case";
      }
    }
  }
}