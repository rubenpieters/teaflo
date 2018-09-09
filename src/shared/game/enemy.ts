import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdEnemy, CreatureId } from "src/shared/game/state";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { TriggerEntityEffect, enemyAbilities, EnemyEffect, damageXAtPos, queueStatus, allAbilities, healSelf } from "src/shared/game/ability";

/*export function showEnemy(
  enemy: Enemy
) {
  return {...enemy,
    actions: enemy.actions.map(showAction),
    triggers: enemy.triggers.map(showTrigger),
  };
}*/

export type Enemy = {
  hp: number,
  ap: number,
  maxHp: number,
  actions: EnemyEffect[],
  triggers: TriggerEntityEffect[],
};

export function damage<E extends Enemy>(
  enemy: E,
  damage: number,
  _piercing: boolean,
): E {
  return focus(enemy,
    over(x => x.hp, x => x - damage),
  );
}

export function act(
  enemy: IdEnemy,
  state: GameState,
  log: Action[],
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: Action[] }  {
  const { action, next } = enemy.actions[enemy.actionIndex].effect(state, { tag: "GlobalId", id: enemy.id, type: "enemy" });
  state = focus(state,
    over(x => x.enemies[index].actionIndex, x => {
      switch (next.tag) {
        case "NextId": {
          const newX = x + 1;
          return newX >= enemy.actions.length ? 0 : newX;
        }
        case "Repeat": {
          return x;
        }
      }
    }),
  );
  return applyActionAndTriggers(action, state, log, idGen, { id: enemy.id, type: "enemy" });
}

export function heal<E extends Enemy>(
  e: E,
  amount: number,
) {
  if (e.hp + amount > e.maxHp) {
    return focus(e, set(x => x.hp, e.maxHp));
  }
  return focus(e, set(x => x.hp, e.hp + amount));
}
/*
const enemyAtk012R10: Enemy = {
  hp: 10,
  maxHp: 10,
  actions: [
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1, 2] },
      value: 5,
      next: { tag: "NextId" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 5,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

const enemyRegenApMinR20: Enemy = {
  hp: 20,
  maxHp: 20,
  actions: [
    {
      tag: "QueueStatus",
      target: { tag: "Self" },
      status: {
        tag: "Regen",
        value: 6,
      },
      next: { tag: "NextId" },
    },
    {
      tag: "DamageAP",
      target: { tag: "All", type: "ally" },
      value: 1,
      next: { tag: "NextId" },
    },
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1, 2] },
      value: 10,
      next: { tag: "NextId" },
    },
  ],
  triggers: [],
};

const enemy8HpAtk2: Enemy = {
  hp: 8,
  maxHp: 8,
  actions: [
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1] },
      value: 7,
      next: { tag: "NextId" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 7,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
    {
      onTag: "Damage",
      type: "before",
      action: {
        tag: "Damage",
        target: { tag: "OriginTarget" },
        value: 1,
      },
      conditions: [
        { tag: "TypeCondition", type: "enemy" },
      ],
    },
  ],
};

const enemy15hpAtk1AllHeal2: Enemy = {
  hp: 15,
  maxHp: 15,
  actions: [
    {
      tag: "Damage",
      target: { tag: "AllCrewPos" },
      value: 1,
      next: { tag: "NextId" },
    },
    {
      tag: "Heal",
      target: { tag: "Self" },
      value: 2,
      next: { tag: "NextId" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 5,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

const enemy14hpApMin: Enemy = {
  hp: 14,
  maxHp: 14,
  actions: [
    {
      tag: "DamageAP",
      target: { tag: "All", type: "ally" },
      value: 1,
      next: { tag: "NextId" },
    },
    {
      tag: "DamageAP",
      target: { tag: "All", type: "ally" },
      value: 1,
      next: { tag: "NextId" },
    },
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1] },
      value: 10,
      next: { tag: "NextId" },
    }
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 7,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

const enemy5HpAtkInFront: Enemy = {
  hp: 5,
  maxHp: 5,
  actions: [
    {
      tag: "ConditionAction",
      conditions: [
        {
          tag: "InPosition",
          position: 0,
        },
      ],
      trueAction: {
        tag: "Damage",
        target: { tag: "Positions", type: "ally", positions: [0] },
        value: 4
      },
      falseAction: { tag: "Noop" },
      next: { tag: "NextId" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 1,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

const enemy20HpDoom: Enemy = {
  hp: 20,
  maxHp: 20,
  actions: [
    {
      tag: "QueueStatus",
      target: { tag: "All", type: "ally" },
      status: {
        tag: "Doom",
        value: 5,
      },
      next: { tag: "NextId" },
    },
    {
      tag: "Noop",
      next: { tag: "Repeat" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 1,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

const enemyAtk2Hp15: Enemy = {
  hp: 15,
  maxHp: 15,
  actions: [
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0] },
      value: 2,
      next: { tag: "NextId" },
    },
  ],
  triggers: [
    {
      onTag: "Death",
      type: "before",
      action: {
        tag: "GainGold",
        gain: 4,
      },
      conditions: [
        { tag: "OwnId" },
      ],
    },
  ],
};

*/

const enemyBoss1: Enemy = {
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
                target: { tag: "Target", type: "ally", position: 0 },
                value: 10,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "Target", type: "ally", position: 1 },
                value: 5,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "Target", type: "ally", position: 2 },
                value: 2,
                piercing: false,
              },
              {
                tag: "Damage",
                target: { tag: "Target", type: "ally", position: 3 },
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


const enemyBoss2: Enemy = {
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


const dummy: Enemy = {
  ap: 1,
  hp: 1000,
  maxHp: 1000,
  actions: [
    enemyAbilities.noopE,
  ],
  triggers: [
  ],
};

const basicEnemy1: Enemy = {
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

const basicEnemy2: Enemy = {
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

export const allEnemies = {
  /*enemyAtk012: enemyAtk012R10,
  enemyRegenApMinR20: enemyRegenApMinR20,
  enemy8HpAtk2: enemy8HpAtk2,
  enemy15hpAtk1AllHeal2: enemy15hpAtk1AllHeal2,
  enemy14hpApMin: enemy14hpApMin,
  enemy5HpAtkInFront: enemy5HpAtkInFront,
  enemy20HpDoom: enemy20HpDoom,
  enemyAtk2Hp15: enemyAtk2Hp15,*/
  enemyBoss1: enemyBoss1,
  enemyBoss2: enemyBoss2,
  dummy: dummy,
  basicEnemy1: basicEnemy1,
  basicEnemy2: basicEnemy2,
};
