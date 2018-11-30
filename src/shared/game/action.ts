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

export type Action
  = Damage
  | Heal
  ;

export function applyAction(
  action: Action,
  state: GameState,
): GameState {
  switch (action.tag) {
    case "Damage": {
      return overUnit(action.target,
        state,
        x => focus(x, over(x => x.hp, x => x - action.value)),
        x => x,
      );
    }
    case "Heal": {
      return overUnit(action.target,
        state,
        x => focus(x, over(x => x.hp, x => x + action.value)),
        x => x,
      );
    }
  }
}