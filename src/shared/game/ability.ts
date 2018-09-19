import { focus, over, set } from "src/shared/iassign-util";
import { TargetType, typeColl } from "src/shared/game/target";
import { Action, AddStatus, highestThreatTarget } from "src/shared/game/action";
import { Target } from "src/shared/game/target";
import { GameState, IdCrew, EntityId, findEntity, CreatureId, toPositionId, inCombat } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";
import { findIndex } from "src/shared/game/trigger";
import { Next } from "src/shared/game/next";
import { Status } from "src/shared/game/status";
import { Enemy } from "src/shared/game/enemy";
import { SolCard } from "src/shared/game/solution";

// TODO: check with gamestate.ts whether given input is self global id or self position index
// (it seems to be position index)

export function createCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): Card {
  return {
    name: "-- created --",
    effects: [
      { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
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

export function createSolCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): SolCard {
  return {
    name: "-- created --",
    effects: [
      { effect: (_state: GameState, _id: CreatureId) => {
        return action;
        },
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

function numberToTarget(input: number): Target {
  if (input >= 0) {
    return { tag: "Target", type: "enemy", position: input }
  } else {
    return { tag: "Target", type: "ally", position: (-input) - 1 }
  }
}

// + additional user input
// entity effect = state, id, type -> state
// player effect = state -> state

// trigger : additional action which triggered

export type EntityEffect = {
  effect: (state: GameState, id: CreatureId) => Action,
  description: string,
  inputs: InputType[],
}

export type EnemyEffect = {
  effect: (state: GameState, id: CreatureId) => { action: Action, next: Next },
  description: string,
}

export type InputEntityEffect = {
  effect: (inputs: any[]) => (state: GameState, id: CreatureId) => Action,
  description: string,
  inputs: InputType[],
}

export type TriggerEntityEffect = {
  effect: (action: Action) => (state: GameState, id: CreatureId) => { action: Action, chargeUse: number },
  description: string,
  charges: number,
  type: "before" | "instead",
}
export type InstanceEffect = {
  effect: (state: GameState, id: CreatureId) => Action,
  description: string,
}

const noop: EntityEffect = {
  effect: (_state: GameState, _id: CreatureId) => {
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

const armorSelf: InputEntityEffect = {
  effect: (_inputs: any[]) => {
    return (state: GameState, id: CreatureId) => {
      if (inCombat(state)) {
        const positionId = toPositionId(state, id);
        const index = positionId.id;
        return addArmor.effect(index, id.type, 1, 8, 0);
      } else {
        return { tag: "Invalid" };
      }
    };
  },
  description: addArmor.description("1T 8", "self"),
  inputs: []
};

const armorDamageToTarget: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (state: GameState, id: CreatureId) => {
      const creature = findEntity(state, id);
      const guard = creature.Guard;
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
    return (state: GameState, id: EntityId) => {
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
    return (_state: GameState, _id: CreatureId) => {
      return damageToTarget.effect(targetPos, "enemy", 15);
    }},
  description: damageToTarget.description("15", "<Target Choice>"),
  inputs: [
    { tag: "NumberInput" },
  ],
};

const dmg10: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, _id: CreatureId) => {
      return damageToTarget.effect(targetPos, "enemy", 10);
    }},
  description: damageToTarget.description("10", "<Target Choice>"),
  inputs: [
    { tag: "NumberInput" },
  ],
};

const addAp: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (state: GameState, _id: CreatureId) => {
      if (inCombat(state)) {
        return {
          tag: "GainAP",
          target: numberToTarget(targetPos),
          value: 1,
        }
      } else {
        return { tag: "Invalid" };
      }
    }},
  description: "add 1 AP to <Target Choice>",
  inputs: [
    { tag: "NumberInput" },
  ],
};

const apDmg: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (state: GameState, id: CreatureId) => {
      const e = findEntity(state, id);
      return damageToTarget.effect(targetPos, "enemy", 5 * e.ap);
    }},
  description: "deal 5 * AP to <Target Choice>",
  inputs: [
    { tag: "NumberInput" },
  ],
};

const dmgPoison: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, _id: CreatureId) => {
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
  dmg10: dmg10,
  dmgPoison: dmgPoison,
  armorSelf: armorSelf,
  addAp: addAp,
  apDmg: apDmg,
};

const armorOnHeal: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      if (action.tag === "Heal" && action.target.position === index) {
        return {
          action: {
            tag: "QueueStatus",
            target: { tag: "Target", type: id.type, position: index, },
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
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      if (action.tag === "AddStatus" && action.status.tag === "Poison" && action.target.position === index) {
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
    return (_state: GameState, _id: CreatureId) => {
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
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
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

const noopE: EnemyEffect = {
  effect: (_state: GameState, _id: CreatureId) => {
    return { action: { tag: "Noop"}, next: { tag: "NextId" } };
  },
  description: "Noop",
};

export function damageHighestThreat(value: number, nextF: (state: GameState, id: CreatureId) => Next): EnemyEffect {
  return {
    effect: (state: GameState, id: CreatureId) => {
      const targetU = highestThreatTarget(id, state);
      const position = targetU === undefined ? 0 : targetU.position;
      return {
        action: {
          tag: "Damage",
          target: { tag: "Target", type: "ally", position },
          value,
          piercing: false,
        },
        next: nextF(state, id),
      };
    },
    description: `deal ${value} to [<highest threat ally>]`,
  };
}

export function damageXAtPos(value: number, position: number): EnemyEffect {
  return {
    effect: (_state: GameState, _id: CreatureId) => {
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
    effect: (state: GameState, _id: CreatureId) => {
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

export function onAllAllyPositions(
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

export function healSelf(value: number): EnemyEffect {
  return {
    effect: (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      return {
        action: {
          tag: "Heal",
          target: { tag: "Target", type: id.type, position: index },
          value,
        },
        next: { tag: "NextId" },
      };
    },
    description: `heal ${value} to [self]`,
  }
};

export const enemyAbilities = {
  noopE: noopE,
  damage_10_0: damageXAtPos(10, 0),
  damage_10_1: damageXAtPos(10, 1),
  damage_10_2: damageXAtPos(10, 2),
  damage_10_3: damageXAtPos(10, 3),
}