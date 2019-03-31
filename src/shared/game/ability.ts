import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { Ability_URI, ActionF } from "./action";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Ability: AbilityVar<A>,
  }
}

/**
 * An Ability is an action with ability variables which still need to be resolved.
 */
export type Ability
  = ActionF<Ability_URI>
  ;

/**
 * An Ability Variable is a description of how to generate the parameters for an action.
 */
type AbilityVar<A>
  = Static<A>
  | FromInput<A>
  ;

export class Static<A> {
  public readonly tag: "Static" = "Static";

  constructor(
    public readonly a: A,
  ) {}
}

export class FromInput<A> {
  public readonly tag: "FromInput" = "FromInput";

  constructor(
    public readonly input: number,
  ) {}
}