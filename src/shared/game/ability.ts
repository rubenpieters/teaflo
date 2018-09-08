import { focus, over, set } from "src/shared/iassign-util";
import { TargetType, typeColl } from "src/shared/game/target";
import { Action, AddStatus } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { GameState, IdCrew } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";
import { findIndex } from "./trigger";
import { Next } from "./next";
import { Status } from "./status";
import { Enemy } from "./enemy";


export function createCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): Card {
  return {
    name: "-- created --",
    effects: [
      { effect: (inputs: any[]) => { return (state: GameState, id: number, type: TargetType) => {
        return action;
        }},
        inputs: [],
        description: "-- created --",
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

export type EnemyEffect = {
  effect: (state: GameState, id: number, type: TargetType) => { action: Action, next: Next },
  description: string,
}

export type InputEntityEffect = {
  effect: (inputs: any[]) => (state: GameState, id: number, type: TargetType) => Action,
  description: string,
  inputs: InputType[],
}

export type TriggerEntityEffect = {
  effect: (action: Action) => (state: GameState, id: number, type: TargetType) => { action: Action, chargeUse: number },
  description: string,
  charges: number,
  type: "before" | "instead",
}

const noop: EntityEffect = {
  effect: (_state: GameState, _id: number, _type: TargetType) => {
    return { tag: "Noop" };
  },
  description: "Noop",
  inputs: [],
};

const damageToTarget = {
  effect: (position: number, type: TargetType, value: number) => {
    return <Action>{
      tag: "Damage",
      target: { tag: "Target", type, position, },
      value,
      piercing: false,
    };
  },
  description: (valueDesc: string, targetDesc: string) => {
    return `deal ${valueDesc} to ${targetDesc}`;
  },
};

const statusToTarget = {
  effect: (position: number, type: TargetType, status: Status["tag"], value: number, fragment: number) => {
    return <Action>{
      tag: "QueueStatus",
      target: { tag: "Target", type, position, },
      status: <Status>{
        tag: status,
        value,
        fragment,
      }
    };
  },
  description: (statusDesc: string, targetDesc: string) => {
    return `apply ${statusDesc} to ${targetDesc}`;
  },
}

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

const dmgPoison: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, _id: number, _type: TargetType) => {
      return {
        tag: "CombinedAction",
        actions: [
          damageToTarget.effect(targetPos, "enemy", 10),
          statusToTarget.effect(targetPos, "enemy", "Poison", 0, 50),
        ],
      }
    }},
  description: `${damageToTarget.description("15", "<Target Choice>")} and ${statusToTarget.description("Poison 50 F", "<Target Choice>")}`,
  inputs: [
    { tag: "NumberInput" },
  ],
};

export const allAbilities = {
  noop: noop,
  armorDamageToTarget: armorDamageToTarget,
  armorAllAlly_5_1_0: armorAllAlly_5_1_0,
  dmg15: dmg15,
  dmgPoison: dmgPoison,
};

const noopE: EnemyEffect = {
  effect: (_state: GameState, _id: number, _type: TargetType) => {
    return { action: { tag: "Noop"}, next: { tag: "NextId" } };
  },
  description: "Noop",
}

const armorOnHeal: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: number, type: TargetType) => {
      const index = findIndex(x => x.id === id, typeColl(state, type));
      if (index === "notFound") {
        throw `not found ${id}`;
      }
      if (action.tag === "Heal" && action.target.position === id) {
        return {
          action: {
            tag: "QueueStatus",
            target: { tag: "Target", type, position: index, },
            status: {
              tag: "Guard",
              value: 1,
              guard: 1,
              fragment: 0,
            },
          },
          chargeUse: 1,
        };
      } else {
        return {
          action: { tag: "Noop" },
          chargeUse: 0,
        }
      }
    };
  },
  description: "gain armor on heal",
  charges: Infinity,
  type: "before",
};

const poisonToPiercing: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: number, type: TargetType) => {
      const index = findIndex(x => x.id === id, typeColl(state, type));
      if (index === "notFound") {
        throw `not found ${id}`;
      }
      if (action.tag === "AddStatus" && action.status.tag === "Poison" && action.target.position === id) {
        return { action: focus(action, set(x => x.status.tag, "PiercingPoison")), chargeUse: 1 };
      } else {
        return { action, chargeUse: 0 };
      }
    };
  },
  description: "convert poison to piercing poison",
  charges: Infinity,
  type: "instead",
};

const regenOnDamage: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: number, type: TargetType) => {
      const index = findIndex(x => x.id === id, typeColl(state, type));
      if (index === "notFound") {
        throw `not found ${id}`;
      }
      if (action.tag === "Damage" && action.target.type === "ally") {
        return {
          action: {
            tag: "QueueStatus",
            target: { tag: "Target", type: "ally", position: action.target.position },
            status: {
              tag: "Regen",
              value: 1,
              fragment: 0,
            },
          },
          chargeUse: 1,
        }
      } else {
        return { action: { tag: "Noop" }, chargeUse: 0 };
      }
    };
  },
  description: "ally gains regen when damaged",
  charges: Infinity,
  type: "before",
};

const interceptAllyDamage: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: number, type: TargetType) => {
      const index = findIndex(x => x.id === id, typeColl(state, type));
      if (index === "notFound") {
        throw `not found ${id}`;
      }
      if (action.tag === "Damage" &&
          action.target.type === "ally" &&
          action.target.position !== index) {
        return {
          action: {
            ...action,
            target: { tag: "Target", type: "ally", position: index },
          },
          chargeUse: 1,
        }
      } else {
        return { action, chargeUse: 0 };
      }
    };
  },
  description: "intercept damage on other allies",
  charges: 1,
  type: "instead",
};

export const allTriggers = {
  armorOnHeal: armorOnHeal,
  poisonToPiercing: poisonToPiercing,
  regenOnDamage: regenOnDamage,
  interceptAllyDamage: interceptAllyDamage,
}


export function damageXAtPos(value: number, position: number): EnemyEffect {
  return {
    effect: (_state: GameState, _id: number, _type: TargetType) => {
      return {
        action: {
          tag: "Damage",
          target: { tag: "Target", type: "ally", position },
          value,
          piercing: false,
        },
        next: { tag: "NextId" },
      };
    },
    description: `deal ${value} to [${position}]`,
  };
}

export function queueStatus(status: Status): EnemyEffect {
  return {
    effect: (state: GameState, _id: number, _type: TargetType) => {
      return {
        action: onAllAllyPositions(
          state,
          (id: number) => { return {
            tag: "QueueStatus",
            target: { tag: "Target", type: "ally", position: id },
            status
          }},
        ),
      next: { tag: "NextId" },
      }
    },
    description: `${JSON.stringify(status)} on all ally positions`,
  };
}

function onAllAllyPositions(
  state: GameState,
  f: (id: number) => Action,
): Action {
  const indices = [...Array(state.crew.length).keys()]
  const actions = indices.map(f);
  return {
    tag: "CombinedAction",
    actions,
  }
}


export const enemyAbilities = {
  noopE: noopE,
  damage_10_0: damageXAtPos(10, 0),
  damage_10_1: damageXAtPos(10, 1),
  damage_10_2: damageXAtPos(10, 2),
  damage_10_3: damageXAtPos(10, 3),
}