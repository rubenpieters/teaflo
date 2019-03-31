/**
 * Every entity has a unique id associated with it.
 * This allows tracking of every entity regardless of its position.
 */
export class EntityId<A extends TargetType>{
  public readonly tag: "EntityId" = "EntityId";

  constructor(
    public readonly id: number,
    public readonly type: A,
  ) {}
}

export function friendlyId(
  id: number,
): FriendlyId {
  return new EntityId(id, "friendly");
}

export function enemyId(
  id: number,
): EnemyId {
  return new EntityId(id, "enemy");
}

export function statusId(
  id: number,
): StatusId {
  return new EntityId(id, "status");
}

/**
 * A unit is belongs to the team which the player controls (friendly).
 * Or to the team which the player fights (enemy).
 */
export type UnitType
  = "friendly"
  | "enemy"
  ;

export const unitTypes: UnitType[] = ["friendly", "enemy"];

export type FriendlyId = EntityId<"friendly">;
export type EnemyId = EntityId<"enemy">;
export type UnitId = EntityId<UnitType>;

/**
 * A target is a unit or a status.
 */
export type TargetType
  = UnitType
  | "status"
  ;

export type StatusId = EntityId<"status">;

export const targetTypes: TargetType[] = (unitTypes as TargetType[]).concat("status");

export type TargetId = EntityId<TargetType>;

/**
 * Specify a unit by its position.
 */
export class PositionId<A extends UnitType>{
  public readonly tag: "PositionId" = "PositionId";

  constructor(
    public readonly id: number,
    public readonly type: A,
  ) {}
}

export type UnitTarget = UnitId | PositionId<UnitType>;

/**
 * An entity has an id attached to it.
 */
export type HasId = {
  id: TargetId,
}
