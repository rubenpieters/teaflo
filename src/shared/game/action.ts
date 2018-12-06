import { focus, over, set } from "src/shared/iassign-util";
import { UnitId, overUnit } from "./entityId";
import { GameState } from "./state";

type Damage = {
  tag: "Damage",
  target: UnitId,
  value: number,
};

export function mkDamage(
  target: UnitId,
  value: number,
): Damage {
  return {
    tag: "Damage",
    target,
    value,
  }
}

type Heal = {
  tag: "Heal",
  target: UnitId,
  value: number,
};

export function mkHeal(
  target: UnitId,
  value: number,
): Heal {
  return {
    tag: "Heal",
    target,
    value,
  }
}

type UseCharge = {
  tag: "UseCharge",
  target: UnitId,
  value: number,
}

export function mkUseCharge(
  target: UnitId,
  value: number,
): UseCharge {
  return {
    tag: "UseCharge",
    target,
    value,
  }
}

type CombinedAction = {
  tag: "CombinedAction",
  actions: Action[],
}

export function mkCombinedAction(
  actions: Action[],
): CombinedAction {
  return {
    tag: "CombinedAction",
    actions,
  }
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