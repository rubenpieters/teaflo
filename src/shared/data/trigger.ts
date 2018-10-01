import { focus, over, set } from "src/shared/iassign-util";
import { Action } from "src/shared/game/action";
import { GameState, CreatureId, toPositionId, IdEnemy } from "src/shared/game/state";
import { TriggerEntityEffect } from "src/shared/game/ability";

/*
export const armorOnHeal: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      if (action.tag === "Heal") {
        const targetPosition = toPositionId(state, action.target).id;
        if (targetPosition === index) {
          return {
            action: {
              tag: "QueueStatus",
              target: id,
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
          };
        }
      } else {
        return {
          action: { tag: "Noop" },
          chargeUse: 0,
        };
      }
    };
  },
  description: "gain armor on heal",
  charges: Infinity,
  type: "before",
};

export const poisonToPiercing: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      if (action.tag === "AddStatus") {
        const targetPosition = toPositionId(state, action.target).id;
        if (action.status.tag === "Poison" && targetPosition === index) {
          return { action: focus(action, set(x => x.status.tag, "PiercingPoison")), chargeUse: 1 };
        } else {
          return { action, chargeUse: 0 };
        }      
      } else {
        return { action, chargeUse: 0 };
      }
    };
  },
  description: "convert poison to piercing poison",
  charges: Infinity,
  type: "instead",
};

export const regenOnDamage: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (_state: GameState, _id: CreatureId) => {
      if (action.tag === "Damage" && action.target.type === "ally") {
        return {
          action: {
            tag: "QueueStatus",
            target: action.target,
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

export const interceptAllyDamage: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: CreatureId) => {
      const positionId = toPositionId(state, id);
      const index = positionId.id;
      if (action.tag === "Damage") {
        // TODO: implement equality of EntityIds and use that
        const targetPosition = toPositionId(state, action.target).id;
        if (action.target.type === "ally" &&
            targetPosition !== index) {
          return {
            action: {
              ...action,
              target: id,
            },
            chargeUse: 1,
          }
        } else {
          return { action, chargeUse: 0 };
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

export const addThreatOnDamage: TriggerEntityEffect = {
  effect: (action: Action) => {
    return (state: GameState, id: CreatureId) => {
      if (action.tag === "Damage" && action.target.type === "ally") {
        return {
          action: onAllEnemy(state, (enemy: IdEnemy, _id: number) => {
          return {
            tag: "AddThreat",
            target: id,
            value: 5,
            enemyId: enemy.id,
          }}),
          chargeUse: 1,
        };
      } else {
        return { action: { tag: "Noop" }, chargeUse: 0 };
      }
    };
  },
  description: "gain threat when ally damaged",
  charges: Infinity,
  type: "before",
};
*/