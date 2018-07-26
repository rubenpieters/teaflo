import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew, IdEnemy } from "src/shared/game/state";
import { ActionTarget, ActionSpec, determineAndApplyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { Target, indexOfId } from "src/shared/game/target";
import { showAction } from "src/shared/game/log";
import { Trigger, showTrigger } from "src/shared/game/trigger";

export function showEnemy(
  enemy: Enemy
) {
  return {...enemy,
    actions: enemy.actions.map(showAction),
    triggers: enemy.triggers.map(showTrigger),
  };
}

export type Enemy = {
  hp: number,
  maxHp: number,
  actions: ActionSpec[],
  triggers: Trigger[],
};

export function damage<E extends Enemy>(
  enemy: E,
  damage: number,
): E {
  return focus(enemy,
    over(x => x.hp, x => x - damage),
  );
}

export function act(
  enemy: IdEnemy,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  const action = enemy.actions[enemy.actionIndex];
  state = focus(state,
    over(x => x.enemies[index].actionIndex, x => {
      const newX = x + 1;
      return newX >= enemy.actions.length ? 0 : newX;
    }),
  );
  return determineAndApplyActionAndTriggers(action, state, log, idGen, enemy.id, "enemy", { id: enemy.id, type: "enemy" });
}

export function heal<E extends Enemy>(
  e: E,
  amount: number,
) {
  if (e.hp + amount > e.maxHp) {
    return focus(e, set(x => x.hp, e.maxHp));
  }
  return focus(e, over(x => x.hp, x => e.hp + amount));
}

const enemyAtk012R10: Enemy = {
  hp: 10,
  maxHp: 10,
  actions: [
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1, 2] },
      value: 5,
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

/*const enemyHeal2R14: Enemy = {
  hp: 14,
  actions: [
    {
      tag: "MeleeAttack",
      multiplier: 1,
      positions: [0],
    },
    {
      tag: "Heal",
      value: 2,
    },
  ],
};*/

const enemyRegenApMinR20: Enemy = {
  hp: 20,
  maxHp: 20,
  actions: [
    {
      tag: "AddStatus",
      target: { tag: "Self" },
      status: {
        tag: "Regen",
        value: 6,
      }
    },
    {
      tag: "DamageAP",
      target: { tag: "All", type: "ally" },
      value: 1,
    },
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1, 2] },
      value: 10
    }
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
      value: 7
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
      value: 1
    },
    {
      tag: "Heal",
      target: { tag: "Self" },
      value: 2
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
    },
    {
      tag: "DamageAP",
      target: { tag: "All", type: "ally" },
      value: 1,
    },
    {
      tag: "Damage",
      target: { tag: "Positions", type: "ally", positions: [0, 1] },
      value: 10
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
      falseAction: { tag: "Noop" }
    }
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
      tag: "AddStatus",
      target: { tag: "All", type: "ally" },
      status: {
        tag: "Doom",
        value: 5,
      },
    },
    {
      tag: "Noop",
    },
    {
      tag: "Noop",
    },
    {
      tag: "Noop",
    },
    {
      tag: "Noop",
    },
    {
      tag: "Noop",
    },
    {
      tag: "Noop",
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

export const allEnemies = {
  enemyAtk012: enemyAtk012R10,
  enemyRegenApMinR20: enemyRegenApMinR20,
  enemy8HpAtk2: enemy8HpAtk2,
  enemy15hpAtk1AllHeal2: enemy15hpAtk1AllHeal2,
  // enemyHeal2R14: enemyHeal2R14,
  enemy14hpApMin: enemy14hpApMin,
  enemy5HpAtkInFront: enemy5HpAtkInFront,
  enemy20HpDoom: enemy20HpDoom,
};
