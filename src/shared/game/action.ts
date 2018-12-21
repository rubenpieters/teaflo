import { focus, over, set } from "src/shared/iassign-util";
import { UnitId, overUnit, overFriendly, killUnit, getUnit } from "./entityId";
import { GameState } from "./state";
import { addThreat } from "./threat";
import { Trigger, loseFragments, addFragments } from "./trigger";
import { damage, heal, useCharge } from "./unit";

export class Damage {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "Damage" = "Damage",
  ) {}
}

export class Heal {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "Heal" = "Heal",
  ) {}
}

export class UseCharge {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "UseCharge" = "UseCharge",
  ) {}
}

export class CombinedAction {
  constructor(
    public readonly actions: Action[],
    public readonly tag: "CombinedAction" = "CombinedAction",
  ) {}
}

export class AddThreat {
  constructor(
    public readonly toFriendly: UnitId,
    public readonly atEnemy: UnitId,
    public readonly value: number,
    public readonly tag: "AddThreat" = "AddThreat",
  ) {}
}

export class AddTrigger {
  constructor(
    public readonly target: UnitId,
    public readonly trigger: Trigger,
    public readonly tag: "AddTrigger" = "AddTrigger",
  ) {}
}

export class LoseFragments {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly triggerTag: Trigger["tag"],
    public readonly tag: "LoseFragments" = "LoseFragments",
  ) {}
}

export class Death {
  constructor(
    public readonly target: UnitId,
    public readonly tag: "Death" = "Death",
  ) {}
}

export type Action
  = Damage
  | Heal
  | UseCharge
  | CombinedAction
  | AddThreat
  | AddTrigger
  | LoseFragments
  | Death
  ;

export function applyAction(
  action: Action,
  state: GameState,
): {
  state: GameState,
  actions: Action[],
} {
  switch (action.tag) {
    case "Damage": {
      state = overUnit(action.target,
        state,
        x => damage(x, action.value),
        x => x,
      );
      const unit = getUnit(action.target, state);
      let actions: Action[] = [];
      if (unit !== undefined && unit.hp <= 0) {
        actions = [
          new Death(action.target),
        ];
      }
      return {
        state,
        actions,
      };
    }
    case "Heal": {
      return {
        state: overUnit(action.target,
          state,
          x => heal(x, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "UseCharge": {
      return {
        state: overUnit(action.target,
          state,
          x => useCharge(x, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "CombinedAction": {
      return {
        state,
        actions: action.actions,
      }
    }
    case "AddThreat": {
      return {
        state: overFriendly(action.toFriendly,
          state,
          x => addThreat(x, state, action.atEnemy, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "AddTrigger": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.triggers, x => addFragments(x, action.trigger))),
          x => x,
        ),
        actions: [],
      };
    }
    case "LoseFragments": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.triggers, x => loseFragments(x, action.triggerTag, action.value))),
          x => x,
        ),
        actions: [],
      };
    }
    case "Death": {
      return {
        state: killUnit(action.target, state),
        actions: [],
      }
    }
  }
}