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
import { routeDirectionDescription } from "./ai";
import deepEqual from "deep-equal";
import { ActionF } from "../definitions/actionf";
import { Ability_URI, Target_URI } from "../definitions/hkt";
import { ActionSource, AbilitySource } from "./log";

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
): (Action & { actionSource: ActionSource })[] {
  const withIndex = ability.map((x, i) => {
    return { ...x, actionSource: new AbilitySource(i) };
  });
  const resolved = withIndex
    .map(x => _resolveAbility(x, state, context))
    .map((l, i) => l.map(x => {
      return {...resolveSingleTargetAbility(x, context), actionSource: new AbilitySource(i) };
    }))
    .reduce((acc, l) => acc.concat(l), []);
  return resolved;
}

function _resolveAbility(
  ability: ActionF<Ability_URI, Target_URI>,
  state: GameState,
  context: Context,
): SingleTargetAbility[] {
  switch (ability.tag) {
    case "Damage": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "Heal": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "UseCharge": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "RestoreCharge": {
      return resolveToSingleTarget(ability, ["target"], state, context);
    }
    case "AddThreat": {
      return resolveToSingleTarget(ability, ["forAlly", "atEnemy"], state, context);
    }
    case "RemoveThreat": {
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

function resolveToSingleTarget<A extends ActionF<Ability_URI, Target_URI>>(
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
  abilities: ActionF<Ability_URI, Target_URI>[],
  field: any,
  resolved: { tag: "ids", ids: TargetId[] } | { tag: "var", var: AbilityVar<A> },
): ActionF<Ability_URI, Target_URI>[] {
  const l: ActionF<Ability_URI, Target_URI>[][] = abilities.map(ability => {
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

function resolveTargetVar<A, B>(
  targetVar: TargetVar<A> | B,
  state: GameState,
  context: Context,
): { tag: "ids", ids: TargetId[] } | { tag: "var", var: B } {
  if (isTargetVar(targetVar)) {
    switch (targetVar.tag) {
      case "AllFriendly": {
        return { tag: "ids", ids: defined(state.frUnits).map(r => r.e.id) };
      }
      case "AllFriendlyExceptSelf": {
        const ids = defined(state.frUnits).map(r => r.e.id)
          .filter(x => ! deepEqual(x, context.self));
        return { tag: "ids", ids };
      }
      case "AllEnemy": {
        return { tag: "ids", ids: defined(state.enUnits).map(r => r.e.id) };
      }
      case "AllEnemyExceptSelf": {
        const ids = defined(state.enUnits).map(r => r.e.id)
          .filter(x => ! deepEqual(x, context.self));
        return { tag: "ids", ids };
      }
      case "AllUnits": {
        const ids: UnitId[] = defined(state.frUnits).map(r => r.e.id as UnitId)
          .concat(defined(state.enUnits).map(r => r.e.id));
        return { tag: "ids", ids };
      }
      case "Self": {
        const self = context.self;
        return { tag: "ids", ids: [self] };
      }
      case "HighestThreat": {
        const self = context.self;
        return { tag: "ids", ids: [getHighestThreat(state, self)] };
      }
      case "LowestHp": {
        return { tag: "ids", ids: [getLowestFriendlyHp(state)] };
      }
      default: {
        return { tag: "var", var: targetVar };
      }
    }
  } else {
    return { tag: "var", var: targetVar };
  }
}

function isTargetVar <A>(a: any): a is TargetVar<A> {
  return a.tag !== undefined;
}

export function getHighestThreat(
  state: GameState,
  self: UnitId,
): UnitId {
  const filtered = frFiltered(state);
  if (filtered.length === 0) {
    throw "getHighestThreat: no friendly unit";
  }
  const firstFr = filtered[filtered.length - 1];
  const threat = filtered.slice(0, filtered.length - 1)
    .reverse()
    .reduce((prev, curr) => {
      if (curr.e.threatMap[self.id] === undefined) {
        return prev;
      }
      if (prev.threatMap[self.id] === undefined) {
        if (curr.e.threatMap[self.id] === 0) {
          return prev;
        } else {
          return curr.e
        }
      }
      if (curr.e.threatMap[self.id] > prev.threatMap[self.id]) {
        return curr.e;
      }
      return prev;
    }, firstFr.e);
  return threat.id;
}

export function getLowestFriendlyHp(
  state: GameState,
): UnitId {
  const filtered = frFiltered(state);
  if (filtered.length === 0) {
    throw "getLowestFriendlyHp: no friendly unit";
  }
  const firstFr = filtered[filtered.length - 1];
  const target = filtered.slice(0, filtered.length - 1)
    .reverse()
    .reduce((prev, curr) => {
      if (curr.e.hp < prev.hp) {
        return curr.e;
      } else {
        return prev;
      }
  }, firstFr.e);
  return target.id;
}

export function abilityDescription(
  ability: Ability,
): DescToken[] {
  const desc: DescToken[] = ability.reduce((acc, x) => {
    if (acc.length === 0) {
      return _abilityDescription(x).concat(acc);
    } else {
      return acc.concat(new DescSeparator()).concat(_abilityDescription(x));
    }
  }, <DescToken[]>[]);
  return desc;
}

function _abilityDescription(
  ability: ActionF<Ability_URI, Target_URI>,
): DescToken[] {
  switch (ability.tag) {
    case "AddThreat": {
      return descSingleton("icon_addthreat")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.forAlly, intentVarTarget))
        .concat(abilityVarDescription(ability.atEnemy, intentVarTarget))
        ;
    }
    case "RemoveThreat": {
      return descSingleton("icon_removethreat")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.forAlly, intentVarTarget))
        .concat(abilityVarDescription(ability.atEnemy, intentVarTarget))
        ;
    }
    case "AddStatus": {
      return descSingleton("icon_addstatus")
        .concat(abilityVarDescription(ability.status, statusDescription))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "Combined": {
      const desc: DescToken[] = ability.list.reduce((acc, x) => {
        if (acc.length === 0) {
          return _abilityDescription(x).concat(acc);
        } else {
          return acc.concat(new DescSeparator()).concat(_abilityDescription(x));
        }
      }, <DescToken[]>[]);
      return desc;
    }
    case "Damage": {
      return descSingleton("icon_damage")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "Heal": {
      return descSingleton("icon_heal")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "UseCharge": {
      return descSingleton("icon_usecharge")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "RestoreCharge": {
      return descSingleton("icon_restorecharge")
        .concat(abilityVarDescription(ability.value, x => numberDescription(x)))
        .concat(abilityVarDescription(ability.target, intentVarTarget))
        ;
    }
    case "MoveAI": {
      return abilityVarDescription(ability.dir, x => routeDirectionDescription(x))
        ;
    }
    case "Death": {
      return descSingleton("icon_death")
        ;
    }
    case "StartTurn": {
      return descSingleton("icon_start_turn")
        ;
    }
    case "Invalid": {
      return descSingleton("icon_invalid")
        ;
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
    case "AllFriendly": {
      return [new DescSymbol("icon_all_friendly")];
    }
    case "AllFriendlyExceptSelf": {
      return [new DescSymbol("icon_all_friendly")];
    }
    case "AllEnemy": {
      return [new DescSymbol("icon_all_enemy")];
    }
    case "AllEnemyExceptSelf": {
      return [new DescSymbol("icon_all_enemy")];
    }
    case "AllUnits": {
      return [new DescSymbol("icon_all_enemy")]
    }
    case "FromInput": {
      return [new DescSymbol("icon_input")];
    }
    case "HighestThreat": {
      return [new DescSymbol("icon_highest_threat")];
    }
    case "LowestHp": {
      return [new DescSymbol("icon_lowesthp")];
    }
    case "Self": {
      return [new DescSymbol("icon_self")];
    }
  }
}

function abilityVarNumber(
  x: number,
  sign: "positive" | "negative",
): DescToken[] {
  if (sign === "positive") {
    return descSingleton("icon_plus").concat(numberDescription(x));
  } else {
    return descSingleton("icon_minus").concat(numberDescription(x));
  }
}

function intentVarTarget(
  x: any
): DescToken[] {
  // TODO: implement
  return [];
}