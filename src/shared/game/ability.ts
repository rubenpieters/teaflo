import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { Ability_URI, ActionF, Target_URI, Damage, Action, UseCharge, Death, Combined, combinedAction, hoistActionF } from "./action";
import { GameState, StFrUnit, StEnUnit } from "./state";
import { UnitRow } from "./unitRow";
import { UnitId, TargetId, EnemyId, FriendlyId } from "./entityId";
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

export function resolveSingleTargetAbility(
  ability: SingleTargetAbility,
  context: Context,
): Action {
  const f = <A>(v: AbilityVar<A>) => resolveAbilityVar(v, context);
  const action = hoistActionF(ability, "Action", "Action", f, f);
  return action;
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
      if (context.tag !== "FrAbilityContext") {
        throw `resolveAbilityVar: Invalid Context ${context.tag}, expected FrAbilityContext`;
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
export type TargetVar<A>
  = AllEnemy
  | AllFriendly
  | Self
  | HighestThreat
  ;

export class AllEnemy {
  public readonly tag: "AllEnemy" = "AllEnemy";

  constructor(

  ) {}
}

export function allEnemy(): TargetVar<EnemyId> {
  return new AllEnemy();
}

export class AllFriendly {
  public readonly tag: "AllAlly" = "AllAlly";

  constructor(

  ) {}
}

export function allFriendly(): TargetVar<FriendlyId> {
  return new AllFriendly();
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

type StateTargetFragment = {
  frUnits: UnitRow<"friendly", StFrUnit>,
  enUnits: UnitRow<"enemy", StEnUnit>,
}

export function resolveAbility(
  ability: Ability,
  state: StateTargetFragment,
  context: Context,
): Action[] {
  const l = _resolveAbility(ability, state);
  return l.map(x => resolveSingleTargetAbility(x, context));
}

function _resolveAbility(
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
    case "AddThreat": {
      // TODO: should also resolve "forAlly" to single target
      return resolveToSingleTarget(ability, "atEnemy", state);
    }
    case "AddStatus": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "MoveAI": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "Death": {
      return resolveToSingleTarget(ability, "target", state);
    }
    case "Invalid": {
      return [ability];
    }
    case "Combined": {
      const l = ability.list.map(x => _resolveAbility(x, state));
      return l.reduce((acc, l) => acc.concat(l), []);
    }
    case "StartTurn": {
      return [ability];
    }
  }
}

function resolveToSingleTarget<A extends Ability>(
  _ability: A,
  field: keyof A,
  state: StateTargetFragment,
): SingleTargetAbility[] {
  const ability = _ability as any;
  const resolved = resolveTargetVar(ability[field], state);
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
    case "Self": {
      throw "TODO: get self from context";
    }
    case "HighestThreat": {
      throw "TODO: get highest threat";
    }
    default: {
      return { tag: "var", var: targetVar };
    }
  }
}