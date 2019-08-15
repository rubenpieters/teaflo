import { ActionF } from "./actionf";
import { Ability_URI, Target_URI } from "./hkt";
import { EnemyId, FriendlyId, TargetId, UnitId } from "./entityId";

/**
 * An Ability is an action with ability variables which still need to be resolved.
 */
export type Ability
  = ActionF<Ability_URI, Target_URI>[]
  ;

/**
 * A SingleTargetAbility is an Ability which has its multiple target parameters resolved.
 */
export type SingleTargetAbility
  = ActionF<Ability_URI, Ability_URI>
  ;

/**
 * An Ability Variable is a description of how to generate the parameters for an action.
 */
export type AbilityVar<A>
  = Static<A>
  | FromInput
  ;

export class Static<A> {
  public readonly tag: "Static" = "Static";

  constructor(
    public readonly a: A,
  ) {}
}

export class FromInput {
  public readonly tag: "FromInput" = "FromInput";

  constructor(
    public readonly input: number,
  ) {}
}

/**
 * A Target Variable contains a description of which targets this ability applies to.
 * This is separated from ability variables since these cause the ability to be decomposed
 * into multiple abilities.
 */
export type TargetVar<A>
  = AllEnemy
  | AllEnemyExceptSelf
  | AllFriendly
  | AllFriendlyExceptSelf
  | AllUnits
  | Self
  | HighestThreat
  | LowestHp
  ;

export class AllEnemy {
  public readonly tag: "AllEnemy" = "AllEnemy";

  constructor(

  ) {}
}

export function allEnemy(): TargetVar<EnemyId> {
  return new AllEnemy();
}

export class AllEnemyExceptSelf {
  public readonly tag: "AllEnemyExceptSelf" = "AllEnemyExceptSelf";

  constructor(

  ) {}
}

export function allEnemyExceptSelf(): TargetVar<EnemyId> {
  return new AllEnemyExceptSelf();
}

export class AllFriendly {
  public readonly tag: "AllFriendly" = "AllFriendly";

  constructor(

  ) {}
}

export function allFriendly(): TargetVar<FriendlyId> {
  return new AllFriendly();
}

export class AllFriendlyExceptSelf {
  public readonly tag: "AllFriendlyExceptSelf" = "AllFriendlyExceptSelf";

  constructor(

  ) {}
}

export function allFriendlyExceptSelf(): TargetVar<FriendlyId> {
  return new AllFriendlyExceptSelf();
}


export class AllUnits {
  public readonly tag: "AllUnits" = "AllUnits";

  constructor(

  ) {}
}

export function allUnits(): TargetVar<UnitId> {
  return new AllUnits();
}

export class Self {
  public readonly tag: "Self" = "Self";

  constructor(

  ) {}
}

export function self(): TargetVar<TargetId> {
  return new Self();
}

export class HighestThreat {
  public readonly tag: "HighestThreat" = "HighestThreat";

  constructor(

  ) {}
}

export function highestThreat(): TargetVar<FriendlyId> {
  return new HighestThreat();
}

export class LowestHp {
  public readonly tag: "LowestHp" = "LowestHp";

  constructor(

  ) {}
}

export function lowestHp(): TargetVar<FriendlyId> {
  return new LowestHp();
}
