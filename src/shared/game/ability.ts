import { hoistActionF } from "./action";
import { frFiltered } from "./state";
import { UnitId, TargetId } from "../definitions/entityId";
import { Context } from "../definitions/context";
import { AbilityVar, SingleTargetAbility, Ability, TargetVar } from "../definitions/ability";
import { Action } from "../definitions/action";
import { GameState, StFrUnit } from "../definitions/state";
import { Static } from "../definitions/condition";

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


export function resolveAbility(
  ability: Ability,
  state: GameState,
  context: Context,
): Action[] {
  const l = _resolveAbility(ability, state, context);
  return l.map(x => resolveSingleTargetAbility(x, context));
}

function _resolveAbility(
  ability: Ability,
  state: GameState,
  context: Context,
): SingleTargetAbility[] {
  switch (ability.tag) {
    case "Damage": {
      return resolveToSingleTarget(ability, "target", state, context);
    }
    case "UseCharge": {
      return resolveToSingleTarget(ability, "target", state, context);
    }
    case "AddThreat": {
      // TODO: should also resolve "forAlly" to single target
      return resolveToSingleTarget(ability, "atEnemy", state, context);
    }
    case "AddStatus": {
      return resolveToSingleTarget(ability, "target", state, context);
    }
    case "MoveAI": {
      return resolveToSingleTarget(ability, "target", state, context);
    }
    case "Death": {
      return resolveToSingleTarget(ability, "target", state, context);
    }
    case "Invalid": {
      return [ability];
    }
    case "Combined": {
      const l = ability.list.map(x => _resolveAbility(x, state, context));
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
  state: GameState,
  context: Context,
): SingleTargetAbility[] {
  const ability = _ability as any;
  const resolved = resolveTargetVar(ability[field], state, context);
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
  state: GameState,
  context: Context,
): { tag: "ids", ids: TargetId[] } | { tag: "var", var: AbilityVar<A> } {
  switch (targetVar.tag) {
    case "AllAlly": {
      return { tag: "ids", ids: state.frUnits.defined().map(r => r.e.id) };
    }
    case "AllEnemy": {
      return { tag: "ids", ids: state.enUnits.defined().map(r => r.e.id) };
    }
    case "Self": {
      const self = context.self;
      return { tag: "ids", ids: [self] };
    }
    case "HighestThreat": {
      const self = context.self;
      return { tag: "ids", ids: [getHighestThreat(state, self)] };
    }
    default: {
      return { tag: "var", var: targetVar };
    }
  }
}

export function getHighestThreat(
  state: GameState,
  self: UnitId,
): UnitId {
  const threat = frFiltered(state)
    .reduce((prev, curr) => {
      if (prev === undefined) {
        return curr.e;
      }
      if (prev.threatMap[self.id] === undefined) {
        return curr.e;
      }
      if (curr.e.threatMap[self.id] === undefined) {
        return prev;
      }
      if (curr.e.threatMap[self.id] >= prev.threatMap[self.id]) {
        return curr.e;
      }
      return prev;
    }, <StFrUnit | undefined>undefined);
  if (threat === undefined) {
    const filtered = frFiltered(state)[0];
    if (filtered !== undefined) {
      return filtered.e.id;
    } else {
      throw "getHighestThreat: no friendly unit";
    }
  } else {
    return threat.id;
  }
}