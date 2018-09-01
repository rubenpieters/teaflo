import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { GameState, IdCrew } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";


export function createCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): Card {
  return {
    name: "-- created --",
    effects: [
      { f: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
        return action;
        }},
        inputs: [],
      },
    ],
    tag: "general",
    origin: {
      tag: "EntityOrigin", 
      entityId: selfId,
      entityType: selfType,
    }
  };
}
// + additional user input
// entity effect = state, id, type -> state
// player effect = state -> state

// trigger : additional action which triggered

export type EntityEffect = {
  effect: (state: GameState, id: number, type: TargetType) => Action,
  description: string,
  inputs: InputType[],
}

export type InputEntityEffect = {
  effect: (inputs: any[]) => (state: GameState, id: number, type: TargetType) => Action,
  description: string,
  inputs: InputType[],
}

const damageToTarget = {
  effect: (position: number, type: TargetType, value: number) => { 
    const action: Action = {
      tag: "Damage",
      target: { tag: "Target", type, position, },
      value,
      piercing: false,
    }
    return action;
  },
  description: (valueDesc: string, targetDesc: string) => {
    return `deal ${valueDesc} to ${targetDesc}`;
  },
};

const addArmor = {
  effect: (position: number, type: TargetType, value: number, guard: number, fragment: number) => {
    const action: Action = {
      tag: "QueueStatus",
      target: { tag: "Target", type, position, },
      status: {
        tag: "Guard",
        value,
        guard,
        fragment,
      },
    }
    return action;
  },
  description: (valueDesc: string, targetDesc: string) => {
    return `add ${valueDesc} armor to ${targetDesc}`
  },
};

const armorDamageToTarget: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (state: GameState, id: number, type: TargetType) => {
      const guard = state.crew[id].Guard;
      const value = guard === undefined ? 0 : guard.guard;
      return damageToTarget.effect(targetPos, "enemy", value);
    }},
  description: damageToTarget.description("<Guard>", "<Target Choice>"),
  inputs: [
    { tag: "NumberInput" },
  ],
};

const armorAllAlly_5_1_0: InputEntityEffect = {
  effect: (_inputs: any[]) => {
    return (state: GameState, _id: number, _type: TargetType) => {
      return onAllAlly(
        state,
        (_: IdCrew, id: number) => {
          return {
            tag: "QueueStatus",
            target: { tag: "Target", type: "ally", position: id, },
            status: {
              tag: "Guard",
              value: 1,
              guard: 5,
              fragment: 0,
            }
          };
        }
      )
    }},
  description: addArmor.description("5 1/0", "all allies"),
  inputs: [],
};

export function onAllAlly(
  state: GameState,
  f: (ally: IdCrew, id: number) => Action,
): Action {
  const actions = state.crew.map(f);
  return {
    tag: "CombinedAction",
    actions,
  }
}

const dmg15: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, _id: number, _type: TargetType) => {
      return damageToTarget.effect(targetPos, "enemy", 15);
    }},
  description: damageToTarget.description("15", "<Target Choice>"),
  inputs: [
    { tag: "NumberInput" },
  ],
};

export const allAbilities = {
  armorDamageToTarget: armorDamageToTarget,
  armorAllAlly_5_1_0: armorAllAlly_5_1_0,
  dmg15: dmg15,
}