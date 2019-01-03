import { focus, over, set } from "src/shared/iassign-util";
import { Action, Damage, LoseFragments, AddTrigger } from "./action";
import { Context } from "./intent";
import { UnitId, eqUnitId, GlobalId, getUnit } from "./entityId";
import { GameState } from "./state";
import { Omit } from "../type-util";

export type HasTriggers = {
  triggers: {
    [K in Trigger["type"]]: Trigger[]
  }
};

export function emptyTriggers() {
  return {
    self: [],
    other: [],
  }
}

export class Weak {
  constructor(
    public readonly hp: number,
    public readonly type: "self",
    public readonly tag: "Weak" = "Weak",
  ) {}
}

export class Strong {
  constructor(
    public readonly hp: number,
    public readonly type: "self",
    public readonly tag: "Strong" = "Strong",
  ) {}
}

export class Armor {
  constructor(
    public readonly hp: number,
    public readonly type: "other",
    public readonly tag: "Armor" = "Armor",
  ) {}
}

export class StrongLowHP {
  constructor(
    public readonly hp: number,
    public readonly type: "self",
    public readonly tag: "StrongLowHP" = "StrongLowHP",
  ) {}
}

export class Grow {
  constructor(
    public readonly hp: number,
    public readonly trigger: Trigger,
    public readonly type: "self",
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

export type TriggerLog = {
  tag: Trigger["tag"],
  before: Action,
  after: Action,
};

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
  // Self Triggers
  for (const frUnit of state.frUnits) {
    if (frUnit !== undefined) {
      for (const trigger of frUnit.triggers.self) {
        const extendedContext = {...context, triggerOwner: new GlobalId(frUnit.id, "friendly")};
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, extendedContext, new GlobalId(frUnit.id, "friendly"));
        action = transformed;
        newActions = newActions.concat(actions);
        if (triggerLog !== undefined) {
          transforms = transforms.concat(triggerLog);
        }
      }
    }
  }

  for (const enUnit of state.enUnits) {
    if (enUnit !== undefined) {
      for (const trigger of enUnit.triggers.self) {
        const extendedContext = {...context, triggerOwner: new GlobalId(enUnit.id, "enemy")};
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, extendedContext, new GlobalId(enUnit.id, "enemy"));
        action = transformed;
        newActions = newActions.concat(actions);
        if (triggerLog !== undefined) {
          transforms = transforms.concat(triggerLog);
        }
      }
    }
  }

  // Other Triggers
  for (const frUnit of state.frUnits) {
    if (frUnit !== undefined) {
      for (const trigger of frUnit.triggers.other) {
        const extendedContext = {...context, triggerOwner: new GlobalId(frUnit.id, "friendly")};
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, extendedContext, new GlobalId(frUnit.id, "friendly"));
        action = transformed;
        newActions = newActions.concat(actions);
        if (triggerLog !== undefined) {
          transforms = transforms.concat(triggerLog);
        }
      }
    }
  }

  for (const enUnit of state.enUnits) {
    if (enUnit !== undefined) {
      for (const trigger of enUnit.triggers.other) {
        const extendedContext = {...context, triggerOwner: new GlobalId(enUnit.id, "enemy")};
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, extendedContext, new GlobalId(enUnit.id, "enemy"));
        action = transformed;
        newActions = newActions.concat(actions);
        if (triggerLog !== undefined) {
          transforms = transforms.concat(triggerLog);
        }
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
  trigger: Trigger,
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
        const subtr = action.value - Math.round((trigger.hp / 100) - 0.5);
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
        const newValue = action.value + Math.round((trigger.hp / 100) - 0.5);
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
        let newValue = action.value - Math.round((trigger.hp / 100) - 0.5);
        newValue = newValue < 0 ? 0 : newValue;
        const transformed = new Damage(
          action.target,
          newValue,
        );
        return {
          transformed,
          actions: [
            new LoseFragments(
              action.target,
              (action.value - newValue) * 100,
              "Armor",
              "other",
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
        const multiplier = Math.round((trigger.hp / 100) - 0.5);
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
      const multiplier = Math.round((trigger.hp / 100) - 0.5);
      const growAction = new AddTrigger(
        owner,
        focus(trigger.trigger, over(x => x.hp, x => x * multiplier)),
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
  triggers: Trigger[],
  trigger: Trigger,
): Trigger[] {
  const index = triggers.findIndex(x => mergeCondition(x, trigger));
  if (index === -1) {
    return triggers.concat(trigger);
  } {
    return focus(triggers,
      over(x => x[index].hp, x => x + trigger.hp),
    );
  }
}

function mergeCondition(
  trigger1: Trigger,
  trigger2: Trigger,
): boolean {
  if (trigger1.tag === "Grow" && trigger2.tag === "Grow" && trigger1.trigger.tag !== trigger2.trigger.tag) {
    return false;
  }
  return trigger1.tag === trigger2.tag;
}

export function loseFragments(
  triggers: Trigger[],
  triggerTag: Trigger["tag"],
  value: number,
): Trigger[] {
  const index = triggers.findIndex(x => x.tag === triggerTag);
  if (index === -1) {
    return triggers;
  } {
    if (triggers[index].hp <= value) {
      return triggers.slice(0, index).concat(triggers.slice(index + 1));
    } else {
      return focus(triggers,
        over(x => x[index].hp, x => x - value),
      );
    }
  }
}