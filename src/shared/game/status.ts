import * as C from "../definitions/condition";
import * as ST from "../definitions/statusTransform";
import { StatusLog } from "./log";
import { StStatus } from "../definitions/statusRow";
import { Status, StatusGroup, StatusTag } from "../definitions/status";
import { Damage, Death } from "../definitions/actionf";
import { ActionWithOrigin } from "../definitions/action";
import { GameState } from "../definitions/state";
import { resolveCondition } from "./condition";
import { resolveStatusTransform } from "./statusTranform";
import { showEntityId } from "./entityId";
import { descSingleton, numberDescription } from "./description";
import { DescSymbol, DescToken } from "../definitions/description";
import { TargetId, UnitId } from "../definitions/entityId";
import { TargetVar } from "../definitions/ability";
import { defined } from "./unitRow";
import { getHighestThreat } from "./ability";
import deepEqual from "deep-equal";

export function statusGroup(
  status: Status,
): StatusGroup {
  switch (status.tag) {
    case "Armor": return "def_mod";
    case "Fragile": return "def_mod";
    case "Strong": return "atk_mod";
    case "Weak": return "atk_mod";
    case "OnDeath": return "other";
  }
}

export const groupOrder: StatusGroup[]
  = ["atk_mod", "def_mod", "other"];

export const statusTags: StatusTag[]
  = ["Weak", "Strong", "Armor", "Fragile"];

export function statusToCondition(
  status: Status,
): C.ActionCondition {
  switch (status.tag) {
    case "Fragile":
    case "Armor": {
      // Dmg Value Target & Origin
      // Dmg (Var 1) (StatusOwner) & (Var 2)
      return {...new Damage("Cond", "Cond", new C.Trivial, C.statusOwner()), origin: new C.Trivial };
    }
    case "Strong":
    case "Weak": {
      // Dmg Value Target & Origin
      // Dmg (Var 1) (Var 2) & (StatusOwner)
      return {...new Damage("Cond", "Cond", new C.Trivial, new C.Trivial), origin: C.statusOwner() };
    }
    case "OnDeath": {
      return {...new Death("Cond", C.statusOwner()), origin: new C.Trivial };
    }
  }
}

export function statusToTransform(
  status: Status,
): {
  transform: ST.StatusTransform,
  actions: ST.StatusAbilityWithOrigin[],
} {
  switch (status.tag) {
    case "Armor": {
      const transform: ST.StatusTransform = {
        ...new Damage("ST", "ST", ST.monus(new ST.Var("value"), C.statusValue()), new ST.Var("target")),
        origin: new ST.Var("origin"),
      };
      const actions: ST.StatusAbilityWithOrigin[] = [
        {
          ...new Death("SATarget", C.idOfStatus()),
          origin: new C.Static("noOrigin") as C.Static<"noOrigin">
        }
      ];
      return { transform, actions };
    }
    case "Weak": {
      const transform: ST.StatusTransform = {
        ...new Damage("ST", "ST", ST.monus(new ST.Var("value"), C.statusValue()), new ST.Var("target")),
        origin: new ST.Var("origin"),
      };
      const actions: ST.StatusAbilityWithOrigin[] = [];
      return { transform, actions };
    }
    case "OnDeath": {
      // copy original action
      const transform: ST.StatusTransform = {
        ...new Death("ST", new ST.Var("target")),
        origin: new ST.Var("origin"),
      };
      const actions: ST.StatusAbilityWithOrigin[] = [
        {
          ...status.ability,
          origin: new C.Static("noOrigin") as C.Static<"noOrigin">,
        }
      ];
      return { transform, actions };
    }
    default: {
      throw "unimpl";
    }
  }
}

export function applyStatuses(
  onStackAction: ActionWithOrigin,
  state: GameState,
): {
  transformed: ActionWithOrigin,
  actions: ActionWithOrigin[],
  transforms: StatusLog[],
} {
  let newActions: ActionWithOrigin[] = [];
  let transforms: StatusLog[] = [];
  for (const group of groupOrder) {
    for (const status of state.statusRows[group].statuses) {
      const { transformed, actions, statusLog } = applyStatus(status, onStackAction, state);
      onStackAction = transformed;
      newActions = newActions.concat(actions);
      if (statusLog !== undefined) {
        transforms = transforms.concat(statusLog);
      }
    }
  }
  return {
    actions: newActions,
    transformed: onStackAction,
    transforms,
  };
}

export function applyStatus(
  status: StStatus,
  onStackAction: ActionWithOrigin,
  state: GameState,
): {
  transformed: ActionWithOrigin,
  actions: ActionWithOrigin[],
  statusLog?: StatusLog,
} {
  const cond = statusToCondition(status);
  const { condition, bindings } = resolveCondition(status, cond, onStackAction);
  if (condition) {
    const st = statusToTransform(status);
    const transformed = resolveStatusTransform(st.transform, bindings, status, onStackAction);
    const actions = st.actions.map(saTarget => {
      const sTransform = _resolveStatusAbilityWithOrigin(saTarget, state, status);
      return sTransform.map(x => resolveStatusTransform(x, bindings, status, onStackAction));
    }).reduce((prev, acc) => acc.concat(prev), []);
    const statusLog = {
      tag: status.tag,
      before: onStackAction,
      after: transformed,
    };
    return { transformed, actions, statusLog };
  } else {
    return { transformed: onStackAction, actions: [] };
  }
}

// TODO: remove duplication with ability.ts

function _resolveStatusAbilityWithOrigin(
  ability: ST.StatusAbilityWithOrigin,
  state: GameState,
  status: StStatus,
): ST.StatusTransform[] {
  switch (ability.tag) {
    case "Damage": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "Heal": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "UseCharge": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "RestoreCharge": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "AddThreat": {
      return resolveToSingleTarget(ability, ["forAlly", "atEnemy"], state, status);
    }
    case "RemoveThreat": {
      return resolveToSingleTarget(ability, ["forAlly", "atEnemy"], state, status);
    }
    case "AddStatus": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "MoveAI": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "Death": {
      return resolveToSingleTarget(ability, ["target"], state, status);
    }
    case "Invalid": {
      return [ability];
    }
    case "Combined": {
      const l = ability.list.map(x => _resolveStatusAbilityWithOrigin(
        { ...x, origin: ability.origin }, state, status
      ));
      return l.reduce((acc, l) => acc.concat(l), []);
    }
    case "StartTurn": {
      return [ability];
    }
  }
}

function resolveToSingleTarget<A extends ST.StatusAbilityWithOrigin>(
  _ability: A,
  fields: (keyof A)[],
  state: GameState,
  status: StStatus,
): ST.StatusTransform[] {
  const ability = _ability as any;
  const resolvedVars = fields.map(field => {
    return { v: resolveTargetVar(ability[field], state, status), field };
  });
  let l: any[] = [ability];
  resolvedVars.forEach(r => {
    l = replaceField(l, r.field, r.v);
  });
  return l;
}

function replaceField<A>(
  abilities: ST.StatusAbilityWithOrigin[],
  field: any,
  resolved: { tag: "ids", ids: TargetId[] } | { tag: "var", var: ST.StatusTransformVar<A> },
): ST.StatusTransform[] {
  const l: ST.StatusTransform[][] = abilities.map(ability => {
    switch (resolved.tag) {
      case "ids": {
        return resolved.ids.map(id => {
          const result: any = {...ability, uriG: "ST" };
          result[field] = new C.Static(id);
          return result;
        });
      }
      case "var": {
        return [{...ability, uriG: "ST" } as any];
      }
    }
  });
  return l.reduce((prev, curr) => prev.concat(curr), []);
}

function resolveTargetVar<A, B>(
  targetVar: TargetVar<A> | B,
  state: GameState,
  status: StStatus,
): { tag: "ids", ids: TargetId[] } | { tag: "var", var: B } {
  if (isTargetVar(targetVar)) {
    switch (targetVar.tag) {
      case "AllFriendly": {
        return { tag: "ids", ids: defined(state.frUnits).map(r => r.e.id) };
      }
      case "AllFriendlyExceptSelf": {
        const ids = defined(state.frUnits).map(r => r.e.id)
          .filter(x => ! deepEqual(x, status.owner));
        return { tag: "ids", ids };
      }
      case "AllEnemy": {
        return { tag: "ids", ids: defined(state.enUnits).map(r => r.e.id) };
      }
      case "AllEnemyExceptSelf": {
        const ids = defined(state.enUnits).map(r => r.e.id)
          .filter(x => ! deepEqual(x, status.owner));
        return { tag: "ids", ids };
      }
      case "AllUnits": {
       const ids: UnitId[] = defined(state.frUnits).map(r => r.e.id as UnitId)
         .concat(defined(state.enUnits).map(r => r.e.id));
       return { tag: "ids", ids };
      }
      case "Self": {
        const self = status.id;
        return { tag: "ids", ids: [self] };
      }
      case "HighestThreat": {
        const self = status.owner;
        return { tag: "ids", ids: [getHighestThreat(state, self)] };
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

export function showStatus(
  status: Status,
): string {
  switch (status.tag) {
    case "Strong": // fallthrough
    case "Weak": // fallthrough
    case "Fragile": // fallthrough
    case "Armor": {
      return `${status.tag}: ${status.value} (${status.hp})`;
    }
    case "OnDeath": {
      return `OnDeath: ${status.value} (${status.hp})`;
    }
  }
}

export function showStStatus(
  status: StStatus,
): string {
  return `ID: ${showEntityId(status.id)} -- ONR: ${showEntityId(status.owner)} -- ${status}`;
}

export function statusDescription(
  status: Status,
): DescToken[] {
  switch (status.tag) {
    case "Weak": {
      return numberDescription(status.value)
        .concat(new DescSymbol("icon_weak"))
        ;
    }
    case "Fragile": {
      return numberDescription(status.value)
        .concat(new DescSymbol("icon_fragile"))
        ;
    }
    case "Strong": {
      return numberDescription(status.value)
        .concat(new DescSymbol("icon_strong"))
        ;
    }
    case "Armor": {
      return numberDescription(status.value)
        .concat(new DescSymbol("icon_armor"))
        ;
    }
    case "OnDeath": {
      return numberDescription(status.value)
        .concat(new DescSymbol("icon_death"))
        ;
    }
  }
}

export function statusTagDescription(
  tag: StatusTag,
): DescToken[] {
  switch (tag) {
    case "Weak": {
      return descSingleton("icon_weak");
    }
    case "Fragile": {
      return descSingleton("icon_fragile");
    }
    case "Strong": {
      return descSingleton("icon_strong");
    }
    case "Armor": {
      return descSingleton("icon_armor");
    }
    case "OnDeath": {
      return descSingleton("icon_death");
    }
  }
}