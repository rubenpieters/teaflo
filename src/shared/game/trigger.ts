import { focus, over, set } from "../iassign-util";
import { Action, Damage, AddTrigger, CombinedAction, AddThreat } from "./action";
import { Context } from "./intent";
import { UnitId, eqUnitId, GlobalId, getUnit, UnitType } from "./entityId";
import { GameState, filteredEn } from "./state";
import { Omit } from "../type-util";
import { HasId } from "./hasId";

export type HasTriggers = {
  triggers: {
    [K in TriggerGroup]: StTrigger[]
  }
};

export type TriggerGroup
  = "weak"
  | "strong"
  | "armor"
  | "other"
  ;

export function emptyTriggers() {
  return {
    self: [],
    other: [],
  }
}

export class Weak {
  constructor(
    public readonly fragments: number,
    public readonly tag: "Weak" = "Weak",
  ) {}
}

export class Strong {
  constructor(
    public readonly fragments: number,
    public readonly tag: "Strong" = "Strong",
  ) {}
}

export class Armor {
  constructor(
    public readonly fragments: number,
    public readonly tag: "Armor" = "Armor",
  ) {}
}

export class Fragile {
  constructor(
    public readonly fragments: number,
    public readonly tag: "Fragile" = "Fragile",
  ) {}
}

export class StrongLowHP {
  constructor(
    public readonly fragments: number,
    public readonly tag: "StrongLowHP" = "StrongLowHP",
  ) {}
}

export class Grow {
  constructor(
    public readonly fragments: number,
    public readonly trigger: Trigger,
    public readonly tag: "Grow" = "Grow",
  ) {}
}

export class AllyWeakSelfArmor {
  constructor(
    public readonly fragments: number,
    public readonly tag: "AllyWeakSelfArmor" = "AllyWeakSelfArmor",
  ) {}
}

export class Explode {
  constructor(
    public readonly value: number,
    public readonly fragments: number,
    public readonly tag: "Explode" = "Explode",
  ) {}
}

export class ThreatOnAllyDamage {
  constructor(
    public readonly fragments: number,
    public readonly tag: "ThreatOnAllyDamage" = "ThreatOnAllyDamage",
  ) {}
}

export type Trigger
  = Weak
  | Strong
  | Armor
  | StrongLowHP
  | Grow
  | AllyWeakSelfArmor
  | Explode
  | ThreatOnAllyDamage
  | Fragile
  ;

function defaultShowTrigger(
  trigger: Trigger,
) {
  return `${trigger.tag}: ${triggerValue(trigger)} (${trigger.fragments})`;
}

export function showTriggerCompact(
  trigger: Trigger,
): string {
  switch (trigger.tag) {
    case "Armor": // fallthrough
    case "Fragile": // fallthrough
    case "Strong": // fallthrough
    case "StrongLowHP": // fallthrough
    case "ThreatOnAllyDamage": // fallthrough
    case "AllyWeakSelfArmor": // fallthrough
    case "Weak": {
      return defaultShowTrigger(trigger);
    }
    case "Grow": {
      return `${defaultShowTrigger(trigger)} ${trigger.tag} - ${showTriggerCompact(trigger.trigger)}`;
    }
    case "Explode": {
      return `${defaultShowTrigger(trigger)} ${trigger.tag} - ${trigger.value}`;
    }
  }
}

export function tagToGroup(
  tag: Trigger["tag"],
): TriggerGroup {
  switch (tag) {
    case "Armor": return "armor";
    case "Grow": return "other";
    case "Strong": return "strong";
    case "StrongLowHP": return "strong";
    case "Weak": return "weak";
    case "AllyWeakSelfArmor": return "other";
    case "Explode": return "other";
    case "ThreatOnAllyDamage": return "other";
    case "Fragile": return "armor";
  }
}

export function triggerToFragmentValue(
  trigger: Trigger,
): number {
  switch (trigger.tag) {
    case "Armor": return 1;
    case "Grow": return 49;
    case "Strong": return 7;
    case "StrongLowHP": return 7;
    case "Weak": return 7;
    case "AllyWeakSelfArmor": return 3;
    case "Explode": return trigger.value;
    case "ThreatOnAllyDamage": return 3;
    case "Fragile": return 1;
  }
}

export function triggerValue(
  trigger: Trigger,
) {
  return Math.round((trigger.fragments / triggerToFragmentValue(trigger)) - 0.5);
}

export function full(
  trigger: Trigger,
): Trigger {
  return focus(trigger,
    over(x => x.fragments, x => x * triggerToFragmentValue(trigger)),
  );
}


export const triggerOrder: TriggerGroup[] = [
  "strong",
  "weak",
  "armor",
  "other",
];

export type TriggerLog = {
  tag: Trigger["tag"],
  before: Action,
  after: Action,
};

export type StTrigger = Trigger & HasId & HasOwner;

export type HasOwner = { owner: GlobalId<UnitType> };

export function applyTriggers(
  state: GameState,
  action: Action,
  context: Context,
): {
  actions: Action[],
  transformed: Action,
  transforms: TriggerLog[],
} {
  let newActions: Action[] = [];
  let transforms: TriggerLog[] = [];
  for (const group of triggerOrder) {
    for (const trigger of state.triggers[group]) {
      const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, context);
      action = transformed;
      newActions = newActions.concat(actions);
      if (triggerLog !== undefined) {
        transforms = transforms.concat(triggerLog);
      }
    }
  }
  return {
    actions: newActions,
    transformed: action,
    transforms,
  };
}

/*
terminology:
SELF - the unit which started the action
TRIGGER OWNER - the unit to which the trigger belongs
ACTION TARGET - the unit on which the action is targeted
*/
export function applyTrigger(
  state: GameState,
  trigger: StTrigger,
  action: Action,
  context: Context,
): {
  actions: Action[],
  transformed: Action,
  triggerLog?: TriggerLog,
} {
  const triggerVal = triggerValue(trigger);
  switch (trigger.tag) {
    case "Weak": {
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, trigger.owner)) {
        const subtr = action.value - triggerVal;;
        const newValue = subtr > 0 ? subtr : 0;
        const transformed = new Damage(
          action.target,
          newValue,
        );
        return {
          transformed,
          actions: [],
          triggerLog: {
            tag: trigger.tag,
            before: action,
            after: transformed,
          },
        };
      } else {
        return { transformed: action, actions: [] };
      }
    }
    case "Strong": {
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, trigger.owner)) {
        const newValue = action.value + triggerVal;
        const transformed = new Damage(
          action.target,
          newValue,
        );
        return {
          transformed,
          actions: [],
          triggerLog: {
            tag: trigger.tag,
            before: action,
            after: transformed,
          },
        };
      } else {
        return { transformed: action, actions: [] };
      }
    }
    case "Armor": {
      if (action.tag === "Damage" && eqUnitId(state, action.target, trigger.owner)) {
        let newValue = action.value - triggerVal;
        newValue = newValue < 0 ? 0 : newValue;
        const transformed = new Damage(
          action.target,
          newValue,
        );
        return {
          transformed,
          actions: [
            new Damage(
              new GlobalId(trigger.id, "status"),
              action.value - newValue,
            ),
          ],
          triggerLog: {
            tag: trigger.tag,
            before: action,
            after: transformed,
          },
        };
      } else {
        return { transformed: action, actions: [] };
      }
    }
    case "StrongLowHP": {
      const self = context.self;
      if (action.tag === "Damage" && self !== undefined && eqUnitId(state, self, trigger.owner)) {
        const multiplier = triggerVal;
        const selfUnit = getUnit(self, state);
        if (selfUnit !== undefined) {
          const missingHp = selfUnit.maxHp - selfUnit.hp;
          const newValue = action.value + missingHp * multiplier * 2;
          const transformed = new Damage(
            action.target,
            newValue,
          );
          return {
            transformed,
            actions: [],
            triggerLog: {
              tag: trigger.tag,
              before: action,
              after: transformed,
            },
          };
        }
      }
      return { transformed: action, actions: [] };
    }
    case "Grow": {
      const multiplier = triggerVal;
      const growAction = new AddTrigger(
        trigger.owner,
        focus(trigger.trigger, over(x => x.fragments, x => x * multiplier)),
      );
      if (action.tag === "StartTurn") {
        return {
          transformed: action,
          actions: [growAction],
        };  
      }
      return { transformed: action, actions: [] };
    }
    case "AllyWeakSelfArmor": {
      const self = context.self;
      if (
        action.tag === "AddTrigger" &&
        tagToGroup(action.trigger.tag) === "weak" &&
        action.target.type === "friendly" &&
        context.self !== undefined &&
        ! eqUnitId(state, action.target, trigger.owner)
      ) {
        const armorAction = new AddTrigger(
          trigger.owner,
          new Armor(triggerVal),
        );
        return {
          transformed: action,
          actions: [armorAction],
        };  
      }
      return { transformed: action, actions: [] };
    }
    case "Explode": {
      if (
        action.tag === "Death" &&
        action.target.type === "status" &&
        action.target.id === trigger.id
      ) {
        const actions = filteredEn(state).map(x => new Damage(new GlobalId(x.id, "enemy"), trigger.value));
        const damageAction = new CombinedAction(actions);
        return { transformed: action, actions: [damageAction] };
      }
      return { transformed: action, actions: [] };
    }
    case "ThreatOnAllyDamage": {
      const self = context.self;
      if (
        action.tag === "Damage" &&
        action.target.type === "friendly" &&
        self !== undefined &&
        ! eqUnitId(state, action.target, trigger.owner)
      ) {
        const actions = filteredEn(state).map(x => new AddThreat(self, new GlobalId(x.id, "enemy"), triggerVal));
        const threatAction = new CombinedAction(actions);
        return { transformed: action, actions: [threatAction] };
      }
      return { transformed: action, actions: [] };
    }
    case "Fragile": {
      if (action.tag === "Damage" && eqUnitId(state, action.target, trigger.owner)) {
        let newValue = action.value + triggerVal;
        const transformed = new Damage(
          action.target,
          newValue,
        );
        return {
          transformed,
          actions: [
            new Damage(
              new GlobalId(trigger.id, "status"),
              newValue - action.value,
            ),
          ],
          triggerLog: {
            tag: trigger.tag,
            before: action,
            after: transformed,
          },
        };
      } else {
        return { transformed: action, actions: [] };
      }
    }
  }
}

export function triggerSprite(
  trigger: Trigger
): string {
  switch (trigger.tag) {
    case "Strong": return "tr_strong";
    case "Weak": return "tr_weak";
    case "Armor": return "tr_armor";
    case "StrongLowHP": return "tr_strong";
    case "Grow": return "tr_strong";
    case "AllyWeakSelfArmor": return "tr_strong";
    case "Explode": return "tr_strong";
    case "ThreatOnAllyDamage": return "tr_strong";
    case "Fragile": return "tr_strong";
  }
}

export function addFragments(
  state: GameState,
  trigger: Trigger & HasOwner,
): GameState {
  const group = tagToGroup(trigger.tag);
  const index = state.triggers[group].findIndex(x => mergeCondition(x, trigger));
  if (index === -1) {
    const nextId = state.nextId;
    const stTr: StTrigger = {...trigger, ...{ id: nextId }};
    return focus(state,
      over(x => x.nextId, x => x + 1),
      over(x => x.triggers[group], x => x.concat(stTr)),
    );
  } {
    return focus(state,
      over(x => x.triggers[group][index].fragments, x => x + trigger.fragments),
    );
  }
}

function mergeCondition(
  trigger1: Trigger & HasOwner,
  trigger2: Trigger & HasOwner,
): boolean {
  if (trigger1.tag === "Grow" && trigger2.tag === "Grow" && trigger1.trigger.tag !== trigger2.trigger.tag) {
    return false;
  }
  return trigger1.tag === trigger2.tag &&
    trigger1.owner.id === trigger2.owner.id &&
    trigger1.owner.type === trigger2.owner.type
    ;
}

export function loseFragments(
  triggers: StTrigger[],
  statusId: GlobalId<"status">,
  value: number,
): StTrigger[] {
  const index = triggers.findIndex(x => x.id === statusId.id);
  if (index === -1) {
    return triggers;
  } {
    if (triggers[index].fragments <= value) {
      return triggers.slice(0, index).concat(triggers.slice(index + 1));
    } else {
      return focus(triggers,
        over(x => x[index].fragments, x => x - value),
      );
    }
  }
}