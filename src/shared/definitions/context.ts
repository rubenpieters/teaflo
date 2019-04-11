import { UnitId } from "./entityId";

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
 * Context when friendly unit has used an ability.
 */
export class EnAbilityContext {
  public readonly tag: "EnAbilityContext" = "EnAbilityContext";

  constructor(
    public readonly self: UnitId,
  ) {}
}

export type Context
  = FrAbilityContext
  | EnAbilityContext
  ;