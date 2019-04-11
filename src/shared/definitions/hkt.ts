import { AbilityVar, TargetVar } from "./ability";
import { ConditionVar } from "./condition";
import { StatusTransformVar } from "./statusTransform";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Action: A,
    Ability: AbilityVar<A>,
    Target: AbilityVar<A> | TargetVar<A>,
    Cond: ConditionVar<A>,
    ST: StatusTransformVar<A>,
  }
}

export type ACTION_URIS = Ability_URI | Action_URI | Target_URI;

export const Ability_URI: "Ability" = "Ability";
export type Ability_URI = typeof Ability_URI;

export const Action_URI: "Action" = "Action";
export type Action_URI = typeof Action_URI;

export const Target_URI: "Target" = "Target";
export type Target_URI = typeof Target_URI;

export const Condition_URI: "Cond" = "Cond";
export type Condition_URI = typeof Condition_URI;

export const StatusTransform_URI: "ST" = "ST";
export type StatusTransform_URI = typeof StatusTransform_URI;
