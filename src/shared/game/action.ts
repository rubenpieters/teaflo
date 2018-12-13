import { focus, over, set } from "src/shared/iassign-util";
import { UnitId, overUnit, overFriendly } from "./entityId";
import { GameState } from "./state";
import { addThreat } from "./threat";

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

export type Action
  = Damage
  | Heal
  | UseCharge
  | CombinedAction
  | AddThreat
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
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.hp, x => x - action.value)),
          x => x,
        ),
        actions: [],
      };
    }
    case "Heal": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.hp, x => x + action.value)),
          x => x,
        ),
        actions: [],
      };
    }
    case "UseCharge": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.charges, x => x - action.value)),
          x => x,
        ),
        actions: [],
      }
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
  }
}