import { focus, over, set } from "src/shared/iassign-util";
import { UnitId, overUnit } from "./entityId";
import { GameState } from "./state";

export class Damage {
  constructor(
    public target: UnitId,
    public value: number,
    public tag: "Damage" = "Damage",
  ) {}
}

export class Heal {
  constructor(
    public target: UnitId,
    public value: number,
    public tag: "Heal" = "Heal",
  ) {}
}

export class UseCharge {
  constructor(
    public target: UnitId,
    public value: number,
    public tag: "UseCharge" = "UseCharge",
  ) {}
}

export class CombinedAction {
  constructor(
    public actions: Action[],
    public tag: "CombinedAction" = "CombinedAction",
  ) {}
}

export type Action
  = Damage
  | Heal
  | UseCharge
  | CombinedAction
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
  }
}