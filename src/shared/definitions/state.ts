import { HasId } from "./entityId";
import { UnitRow } from "./unitRow";
import { StatusGroup } from "./status";
import { StatusRow } from "./statusRow";
import { ThreatMap } from "./threat";
import { FrUnit, EnUnit } from "./unit";

export type StFrUnit = FrUnit & HasId<"friendly"> & { threatMap: ThreatMap };
export type StEnUnit = EnUnit & HasId<"enemy">;

export type StateType
  = "invalid"
  | "win"
  | "default"
  ;

export class GameState {
  constructor(
    public readonly frUnits: UnitRow<"friendly", StFrUnit>,
    public readonly enUnits: UnitRow<"enemy", StEnUnit>,
    public readonly nextId: number = 0,
    public readonly statusRows: { [K in StatusGroup]: StatusRow },
    public readonly type: StateType = "default",
  ) {}
}