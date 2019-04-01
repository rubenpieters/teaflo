import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { Ability_URI, ActionF, Target_URI, Damage, Action, UseCharge, Death, Combined, CombinedAction } from "./action";
import { GameState, StFrUnit, StEnUnit } from "./state";
import { UnitRow } from "./unitRow";
import { UnitId, TargetId } from "./entityId";
import { Context } from "./context";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Ability: AbilityVar<A>,
    Target: AbilityVar<A> | TargetVar<A>,
  }
}

/**
 * An Ability is an action with ability variables which still need to be resolved.
 */
export type Ability
  = ActionF<Ability_URI, Target_URI>
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
type AbilityVar<A>
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

export function resolveSingleTargetAbility(
  ability: SingleTargetAbility,
  context: Context,
): Action {
  switch (ability.tag) {
    case "Damage": {
      const rValue = resolveAbilityVar(ability.value, context);
      const rTarget = resolveAbilityVar(ability.target, context);
      return new Damage("Action", "Action", rValue, rTarget);
    }
    case "UseCharge": {
      const rValue = resolveAbilityVar(ability.value, context);
      const rTarget = resolveAbilityVar(ability.target, context);
      return new UseCharge("Action", "Action", rValue, rTarget);
    }
    case "Death": {
      const rTarget = resolveAbilityVar(ability.target, context);
      return new Death("Action", rTarget);
    }
    case "Invalid": {
      return ability;
    }
    case "Combined": {
      const l = ability.list.map(x => resolveSingleTargetAbility(x, context));
      return CombinedAction(l);
    }
  }
}

function resolveAbilityVar<A>(
  abilityVar: AbilityVar<A>,
  context: Context,
): A {
  switch (abilityVar.tag) {
    case "Static": {
      return abilityVar.a;
    }
    case "FromInput": {
      if (context.input === undefined) {
        throw "resolveAbilityVar: no input for FromInput Ability";
      }
      if (context.input[abilityVar.input]) {
        throw `resolveAbilityVar: no input at ${abilityVar.input} for FromInput Ability`;
      }
      return context.input[abilityVar.input];
    }
  }
}

/**
 * A Target Variable contains a description of which targets this ability applies to.
 * This is separated from ability variables since these cause the ability to be decomposed
 * into multiple abilities.
 */
type TargetVar<A>
  = AllEnemy
  | AllAlly
  ;

export class AllEnemy {
  public readonly tag: "AllEnemy" = "AllEnemy";

  constructor(

  ) {}
}

export class AllAlly {
  public readonly tag: "AllAlly" = "AllAlly";

  constructor(

  ) {}
}

type StateTargetFragment = {
  frUnits: UnitRow<"friendly", StFrUnit>,
  enUnits: UnitRow<"enemy", StEnUnit>,
}

export function resolveAbility(
  ability: Ability,
  state: StateTargetFragment,
): SingleTargetAbility[] {
  switch (ability.tag) {
    case "Damage": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "UseCharge": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "Death": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "Invalid": {
      return [ability];
    }
    case "Combined": {
      const l = ability.list.map(x => resolveAbility(x, state));
      return l.reduce((acc, l) => acc.concat(l), []);
    }
  }
}

function resolveToSingleTarget(
  ability: Ability,
  field: string,
  state: StateTargetFragment,
): SingleTargetAbility[] {
  const resolved = resolveTargetVar((ability as any)[field], state);
  switch (resolved.tag) {
    case "ids": {
      return resolved.ids.map(id => {
        const result: any = {...ability, uriG: "Ability" };
        result[field] = new Static(id);
        return result;
      });
    }
    case "var": {
      return [{...ability, uriG: "Ability" } as any];
    }
  }
}

function resolveTargetVar<A>(
  targetVar: TargetVar<A> | AbilityVar<A>,
  state: StateTargetFragment,
): { tag: "ids", ids: TargetId[] } | { tag: "var", var: AbilityVar<A> } {
  switch (targetVar.tag) {
    case "AllAlly": {
      return { tag: "ids", ids: state.frUnits.defined().map(r => r.e.id) };
    }
    case "AllEnemy": {
      return { tag: "ids", ids: state.enUnits.defined().map(r => r.e.id) };
    }
    default: {
      return { tag: "var", var: targetVar };
    }
  }
}