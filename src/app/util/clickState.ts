import { Ability } from "../../shared/game/ability";
import { UnitId } from "src/shared/game/entityId";

export type ClickState = {
  ability: Ability,
  origin: UnitId | undefined;
  currentInputs: any[],
}