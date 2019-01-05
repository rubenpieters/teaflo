import { focus, over, set } from "src/shared/iassign-util";
import { Action, Damage, AddTrigger } from "./action";
import { Context } from "./intent";
import { UnitId, eqUnitId, GlobalId, getUnit, UnitType } from "./entityId";
import { GameState } from "./state";
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

export type Trigger
  = Weak
  | Strong
  | Armor
  | StrongLowHP
  | Grow
  ;

export function tagToGroup(
  tag: Trigger["tag"],
): TriggerGroup {
  switch (tag) {
    case "Armor": return "armor";
    case "Grow": return "other";
    case "Strong": return "strong";
    case "StrongLowHP": return "strong";
    case "Weak": return "weak";
  }
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
      const extendedContext = {...context, triggerOwner: trigger.owner };
      const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, extendedContext, trigger.owner);
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

export function applyTrigger(
  state: GameState,
  trigger: StTrigger,
  action: Action,
  context: Context,
  transformSelf: UnitId,
): {
  actions: Action[],
  transformed: Action,
  triggerLog?: TriggerLog,
} {
  switch (trigger.tag) {
    case "Weak": {
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, transformSelf)) {
        const subtr = action.value - Math.round((trigger.fragments / 100) - 0.5);
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
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, transformSelf)) {
        const newValue = action.value + Math.round((trigger.fragments / 100) - 0.5);
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
      if (action.tag === "Damage" && eqUnitId(state, action.target, transformSelf)) {
        let newValue = action.value - Math.round((trigger.fragments / 100) - 0.5);
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
      if (action.tag === "Damage" && self !== undefined && eqUnitId(state, self, transformSelf)) {
        const multiplier = Math.round((trigger.fragments / 100) - 0.5);
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
      const owner = context.triggerOwner;
      if (owner === undefined) {
        throw "applyTrigger: no trigger owner";
      }
      const multiplier = Math.round((trigger.fragments / 100) - 0.5);
      const growAction = new AddTrigger(
        owner,
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