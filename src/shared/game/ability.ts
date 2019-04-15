import { hoistActionF } from "./action";
import { frFiltered } from "./state";
import { UnitId, TargetId } from "../definitions/entityId";
import { Context } from "../definitions/context";
import { AbilityVar, SingleTargetAbility, Ability, TargetVar } from "../definitions/ability";
import { Action } from "../definitions/action";
import { GameState, StFrUnit } from "../definitions/state";
import { Static } from "../definitions/condition";
import { defined } from "./unitRow";
import { DescToken, DescSymbol, DescSeparator } from "../definitions/description";
import { descSingleton, numberDescription } from "./description";
import { statusDescription } from "./status";

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
      if (context.input[abilityVar.input] === undefined) {
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
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "UseCharge": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "AddThreat": {
      return resolveToSingleTarget(ability, ["forAlly", "atEnemy"], state, context);
    }
    case "AddStatus": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "MoveAI": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "Death": {
      return resolveToSingleTarget(ability, ["target"], state, context);
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
  fields: (keyof A)[],
  state: GameState,
  context: Context,
): SingleTargetAbility[] {
  const ability = _ability as any;
  const resolvedVars = fields.map(field => {
    return { v: resolveTargetVar(ability[field], state, context), field };
  });
  let l: any[] = [ability];
  resolvedVars.forEach(r => {
    l = replaceField(l, r.field, r.v);
  });
  return l;
}

function replaceField<A>(
  abilities: Ability[],
  field: any,
  resolved: { tag: "ids", ids: TargetId[] } | { tag: "var", var: AbilityVar<A> },
): Ability[] {
  const l: Ability[][] = abilities.map(ability => {
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
  });
  return l.reduce((prev, curr) => prev.concat(curr), []);
}

function resolveTargetVar<A>(
  targetVar: TargetVar<A> | AbilityVar<A>,
  state: GameState,
  context: Context,
): { tag: "ids", ids: TargetId[] } | { tag: "var", var: AbilityVar<A> } {
  switch (targetVar.tag) {
    case "AllAlly": {
      return { tag: "ids", ids: defined(state.frUnits).map(r => r.e.id) };
    }
    case "AllEnemy": {
      return { tag: "ids", ids: defined(state.enUnits).map(r => r.e.id) };
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

export function abilityDescription(
  ability: Ability,
): DescToken[] {
  switch (ability.tag) {
    case "AddThreat": {
      return abilityVarDescription(ability.value, x => abilityVarNumber(x, "positive"))
        .concat(abilityVarDescription(ability.atEnemy, intentVarTarget))
        .concat(new DescSymbol("icon_th"))
        .concat(abilityVarDescription(ability.forAlly, intentVarTarget))
        ;
    }
    case "AddStatus": {
      return abilityVarDescription(ability.status, statusDescription)
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "Combined": {
      const desc: DescToken[] = ability.list.reduce((acc, x) => {
        if (acc.length === 0) {
          return acc.concat(abilityDescription(x));
        } else {
          return acc.concat(new DescSeparator()).concat(abilityDescription(x));
        }
      }, <DescToken[]>[]);
      return desc;
    }
    case "Damage": {
      return abilityVarDescription(ability.value, x => abilityVarNumber(x, "negative"))
        .concat(new DescSymbol("icon_hp"))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "UseCharge": {
      return abilityVarDescription(ability.value, x => abilityVarNumber(x, "negative"))
        .concat(new DescSymbol("icon_ch"))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "MoveAI": {
      return descSingleton("icon_ai");
    }
    default: {
      throw `unimpl: ${JSON.stringify(ability)}`;
    }
  }
}

export function abilityVarDescription<A>(
  abilityVar: TargetVar<A> | AbilityVar<A>,
  f: (a: A) => DescToken[]
): DescToken[] {
  switch (abilityVar.tag) {
    case "Static": {
      return f(abilityVar.a);
    }
    case "AllAlly": {
      return [new DescSymbol("expl_all_friendly")];
    }
    case "AllEnemy": {
      return [new DescSymbol("expl_all_enemy")];
    }
    case "FromInput": {
      return [new DescSymbol("expl_target")];
    }
    case "HighestThreat": {
      return [new DescSymbol("expl_target_status")];
    }
    case "Self": {
      return [new DescSymbol("expl_self")];
    }
  }
}

function abilityVarNumber(
  x: number,
  sign: "positive" | "negative",
): DescToken[] {
  if (sign === "positive") {
    return descSingleton("expl_plus").concat(numberDescription(x));
  } else {
    return descSingleton("expl_minus").concat(numberDescription(x));
  }
}

function intentVarTarget(
  x: any
): DescToken[] {
  // TODO: implement
  return [];
}