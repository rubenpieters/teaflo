import { UnitId } from "./entityId";
import { Action } from "./action";
import { Status } from "./status";

/**
 * Context when friendly unit has used an ability.
 */
export class FrAbilityContext {
  public readonly tag: "FrAbilityContext" = "FrAbilityContext";

  constructor(
    public readonly self: UnitId,
    public readonly input: any[],
  ) {}
}

/**
 * Context when a status effect is executed.
 */
export class StatusContext {
  public readonly tag: "StatusContext" = "StatusContext";

  constructor(
    public readonly owner: UnitId,
    public readonly status: Status,
  ) {}
}

export type Context
  = FrAbilityContext
  | StatusContext
  ;