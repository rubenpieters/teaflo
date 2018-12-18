import { Action, Damage } from "./action";
import { Context } from "./intent";
import { UnitId, eqUnitId, GlobalId } from "./entityId";
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

export type Trigger
  = Weak
  | Strong
  | Armor
  ;

export function applyTriggers(
  state: GameState,
  action: Action,
  context: Context,
) {
  for (const frUnit of state.frUnits) {
    if (frUnit !== undefined) {
      for (const trigger of frUnit.triggers) {
        action = applyTrigger(state, trigger, action, context, new GlobalId(frUnit.id, "friendly"));
      }
    }
  }

  for (const enUnit of state.enUnits) {
    if (enUnit !== undefined) {
      for (const trigger of enUnit.triggers) {
        action = applyTrigger(state, trigger, action, context, new GlobalId(enUnit.id, "enemy"));
      }
    }
  }
  return action;
}

export function applyTrigger(
  state: GameState,
  trigger: Trigger,
  action: Action,
  context: Context,
  transformSelf: UnitId,
): Action {
  switch (trigger.tag) {
    case "Weak": {
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, transformSelf)) {
        const subtr = action.value - Math.round((trigger.fragments / 100) - 0.5);
        const newValue = subtr > 0 ? subtr : 0;
        return new Damage(
          action.target,
          newValue,
        );
      } else {
        return action;
      }
    }
    case "Strong": {
      if (action.tag === "Damage" && context.self !== undefined && eqUnitId(state, context.self, transformSelf)) {
        const newValue = action.value + Math.round((trigger.fragments / 100) - 0.5);
        return new Damage(
          action.target,
          newValue,
        );
      } else {
        return action;
      }
    }
    case "Armor": {
      if (action.tag === "Damage" && eqUnitId(state, action.target, transformSelf)) {
        const newValue = action.value - Math.round((trigger.fragments / 100) - 0.5);
        return new Damage(
          action.target,
          newValue,
        );
      } else {
        return action;
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
  }
}