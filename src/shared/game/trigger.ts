import { focus, over, set } from "src/shared/iassign-util";
import { Action, Damage, LoseFragments } from "./action";
import { Context } from "./intent";
import { UnitId, eqUnitId, GlobalId, getUnit } from "./entityId";
import { GameState } from "./state";

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

export type Trigger
  = Weak
  | Strong
  | Armor
  | StrongLowHP
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
  for (const frUnit of state.frUnits) {
    if (frUnit !== undefined) {
      for (const trigger of frUnit.triggers) {
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, context, new GlobalId(frUnit.id, "friendly"));
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
      for (const trigger of enUnit.triggers) {
        const { actions, transformed, triggerLog } = applyTrigger(state, trigger, action, context, new GlobalId(enUnit.id, "enemy"));
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
            new LoseFragments(
              action.target,
              (action.value - newValue) * 100,
              "Armor",
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
  }
}

export function addFragments(
  triggers: Trigger[],
  trigger: Trigger,
) {
  const index = triggers.findIndex(x => x.tag === trigger.tag);
  if (index === -1) {
    return triggers.concat(trigger);
  } {
    return focus(triggers,
      over(x => x[index].fragments, x => x + trigger.fragments),
    );
  }
}

export function loseFragments(
  triggers: Trigger[],
  triggerTag: Trigger["tag"],
  value: number,
) {
  const index = triggers.findIndex(x => x.tag === triggerTag);
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