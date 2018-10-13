import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action, highestThreatTarget, Damage } from "src/shared/game/action";
import { GameState, IdCrew, EntityId, findEntity, CreatureId, toPositionId, inCombat, IdEnemy, showId, PositionIdA } from "src/shared/game/state";
import { Next } from "src/shared/game/next";
import { Status } from "src/shared/game/status";
import { InputEntityEffect, EnemyEffect, EntityEffect } from "src/shared/game/ability";
import { Eff1 } from "../game/effectvar";

/*
function numberToTarget(input: number): CreatureId {
  if (input >= 0) {
    return { tag: "PositionId", type: "enemy", id: input }
  } else {
    return { tag: "PositionId", type: "ally", id: (-input) - 1 }
  }
}


const damageToTarget = {
  effect: (position: number, type: TargetType, value: number) => {
    return <Action>{
      tag: "Damage",
      target: { tag: "PositionId", type, id: position, },
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
    return {
      tag: "QueueStatus",
      target: { tag: "PositionId", type, id: position, },
      status: {
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
  effect: (position: number, type: "ally" | "enemy", value: number, guard: number, fragment: number) => {
    const action: Action = {
      tag: "QueueStatus",
      target: { tag: "PositionId", type, id: position, },
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

export const armorSelf: InputEntityEffect = {
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

export const armorDamageToTarget: InputEntityEffect = {
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

export const armorAllAlly_5_1_0: InputEntityEffect = {
  effect: (_inputs: any[]) => {
    return (state: GameState, id: EntityId) => {
      return onAllAlly(
        state,
        (_ally, id) => {
          return {
            tag: "QueueStatus",
            target: id,
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

export const dmg15: InputEntityEffect = {
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

export const dmg10: InputEntityEffect = {
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

export const addAp: InputEntityEffect = {
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

export const apDmg: InputEntityEffect = {
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

export const dmgPoison: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, _id: CreatureId) => {
      return <Action>{
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

export const dmgAllGainBubble: InputEntityEffect = {
  effect: (_inputs: any[]) => {
    return (state: GameState, selfId: CreatureId) => {
      return {
        tag: "CombinedAction",
        actions: [
          onAllAlly(state, (_ally, id) => {
            return {
              tag: "Damage",
              target: id,
              value: 5,
              piercing: false,
            }
          }),
          {
            tag: "QueueStatus",
            target: selfId,
            status: {
              tag: "Bubble",
              value: 1,
              fragment: 0,
            }
          },
        ]
      }
    }},
  description: "dmg all allies, gain bubble",
  inputs: [
  ],
};

export const dmgSelfHealAllies: InputEntityEffect = {
  effect: (inputs: any[]) => {
    return (state: GameState, selfId: CreatureId) => {
      const positionId = toPositionId(state, selfId);
      const selfPosition = positionId.id;
      return {
        tag: "CombinedAction",
        actions: [
          {
            tag: "Damage",
            target: selfId,
            value: 20,
            piercing: false,
          },
          onAllAlly(state, (_ally: IdCrew, id) => {
            if (id.id === selfPosition) {
              return { tag: "Noop" };
            } else {
              return {
                tag: "Heal",
                target: id,
                value: 30,
                piercing: false,
              }
            }
          }),
        ]
      }
    }},
  description: "dmg self, heal allies",
  inputs: [
  ],
};

export const armorSelf10_2: InputEntityEffect = {
  effect: (_inputs: any[]) => {
    return (_state: GameState, selfId: CreatureId) => {
      return {
        tag: "QueueStatus",
        target: selfId,
        status: {
          tag: "Guard",
          value: 2,
          guard: 10,
          fragment: 0,
        },
      }
    }},
  description: "armor self 10 2",
  inputs: [
  ],
};

export const dmgHighCd: InputEntityEffect = {
  effect: (inputs: any[]) => {
    const targetPos: number = inputs[0];
    return (_state: GameState, selfId: CreatureId) => {
      return {
        tag: "CombinedAction",
        actions: [
          {
            tag: "ChargeUse",
            value: 4,
            target: selfId,
          },
          damageToTarget.effect(targetPos, "enemy", 10),
        ]
      }
    }},
  description: damageToTarget.description("25", "<Target Choice>"),
  inputs: [
    { tag: "NumberInput" },
  ],
};

export const noopE: EnemyEffect = {
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
          target: { tag: "PositionId", type: "ally", id: position },
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
          target: { tag: "PositionId", type: "ally", id: position },
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
            target: { tag: "PositionId", type: "ally", id },
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

export function onAllAlly(
  state: GameState,
  f: (ally: IdCrew, id: PositionIdA<"ally">) => Action,
): Action {
  const actions = state.crew.map((ally, id) =>
    f(ally, { tag: "PositionId", type: "ally", id })
  );
  return {
    tag: "CombinedAction",
    actions,
  }
}

export function onAllEnemy(
  state: GameState,
  f: (enemy: IdEnemy, id: number) => Action,
): Action {
  const actions = state.enemies.map(f);
  return {
    tag: "CombinedAction",
    actions,
  }
}

export function healSelf(value: number): EnemyEffect {
  return {
    effect: (state: GameState, id: CreatureId) => {
      return {
        action: {
          tag: "Heal",
          target: id,
          value,
        },
        next: { tag: "NextId" },
      };
    },
    description: `heal ${value} to [self]`,
  }
};
*/