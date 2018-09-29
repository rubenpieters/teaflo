import { focus, over, set } from "src/shared/iassign-util";
import { GameState, CreatureId, toPositionId } from "src/shared/game/state";
import { damageXAtPos, queueStatus, healSelf, damageHighestThreat, onAllAllyPositions } from "src/shared/data/ability";
import { Next } from "src/shared/game/next";
import { Enemy } from "src/shared/game/enemy";

export const enemyBoss1: Enemy = {
  ap: 1,
  hp: 40,
  maxHp: 40,
  actions: [
    queueStatus({
      tag: "Poison",
      value: 3,
      fragment: 0,
    }),
    queueStatus({
      tag: "Blind",
      value: 4,
      fragment: 0,
    }),
    queueStatus({
      tag: "Silence",
      value: 3,
      fragment: 0,
    }),
    {
      effect: (state: GameState, id: CreatureId) => {
        return {
          action: {
            tag: "CombinedAction",
            actions: [
              {
                tag: "Damage",
                target: { tag: "PositionId", type: "ally", id: 0 },
                value: 10,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "PositionId", type: "ally", id: 1 },
                value: 5,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "PositionId", type: "ally", id: 2 },
                value: 2,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "PositionId", type: "ally", id: 3 },
                value: 1,
                piercing: false,
              },
            ],
          },
          next: { tag: "NextId" },
        };
      },
      description: "1 | 2 | 5 | 10 dmg",
    }
  ],
  triggers: [
  ],
};

export const enemyBoss2: Enemy = {
  ap: 1,
  hp: 40,
  maxHp: 40,
  actions: [
    damageXAtPos(10, 0),
    damageXAtPos(10, 1),
    damageXAtPos(10, 2),
    damageXAtPos(10, 3),
  ],
  triggers: [
  ],
};

export const enemyBoss3: Enemy = {
  ap: 1,
  hp: 40,
  maxHp: 40,
  actions: [
    {
      effect: (state: GameState, id: CreatureId) => {
        const positionId = toPositionId(state, id);
        const index = positionId.id;
        let next: Next;
        if (state.enemies[index].hp / state.enemies[index].maxHp < 0.3) {
          next = { tag: "Goto", action: 1 };
        } else {
          next = { tag: "Goto", action: 0 };
        }
        return {
          action: onAllAllyPositions(state, id => {
            return {
              tag: "QueueStatus",
              target: { tag: "PositionId", type: "ally", id },
              status: {
                tag: "Poison",
                value: 2,
                fragment: 50,
              }
            };
          }),
          next,
        };
      },
      description: `poison all allies`,
    },
    healSelf(10),
  ],
  triggers: [
  ],
};

export const dummy: Enemy = {
  ap: 1,
  hp: 1000,
  maxHp: 1000,
  actions: [
    damageHighestThreat(1, _ => { return { tag: "NextId" }}),
  ],
  triggers: [
  ],
};

export const basicEnemy1: Enemy = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  actions: [
    damageXAtPos(10, 0),
    healSelf(10),
  ],
  triggers: [
  ],
};

export const basicEnemy2: Enemy = {
  ap: 1,
  hp: 20,
  maxHp: 20,
  actions: [
    damageXAtPos(5, 0),
    healSelf(10),
  ],
  triggers: [
  ],
};