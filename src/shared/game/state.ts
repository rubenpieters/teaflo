import { FrUnit, EnUnit } from "./unit";
import { HasId, TargetId, TargetType, EntityId, FriendlyId, EnemyId, StatusId } from "./entityId";
import { UnitRow } from "./unitRow";
import { StatusRow, StStatus } from "./statusRow";
import { focus, over } from "../iassign-util";
import { damageEntity } from "./entity";

export type StFrUnit = FrUnit & HasId;
export type StEnUnit = EnUnit & HasId;

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
    target: EntityId<Type>,
    f: (e: IdToEntityType[Type]) => IdToEntityType[Type],
  ) {
    switch (target.type) {
      case "friendly": {
        // compiler does not refine the types for us
        const frTarget = target as FriendlyId;
        const frF = f as (e: StFrUnit) => StFrUnit;

        return focus(this,
          over(x => x.frUnits, x => x.overUnit(frTarget, frF)),
        );
      }
      case "enemy": {
        // compiler does not refine the types for us
        const enTarget = target as EnemyId;
        const enF = f as (e: StEnUnit) => StEnUnit;

        return focus(this,
          over(x => x.enUnits, x => x.overUnit(enTarget, enF)),
        );
      }
      case "status": {
        // compiler does not refine the types for us
        const stTarget = target as StatusId;
        const stF = f as (e: StStatus) => StStatus;

        return focus(this,
          over(x => x.statusRow, x => x.overStatus(stTarget, stF)),
        );
      }
      default: {
        throw "GameState.overTarget: impossible case";
      }
    }
  }
}