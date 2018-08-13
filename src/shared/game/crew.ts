import { focus, over, set } from "src/shared/iassign-util";
import { Trigger, showTrigger } from "src/shared/game/trigger";
import { GameState, IdCrew } from "src/shared/game/state";
import { ActionTarget, ActionSpec, determineAndApplyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { HasStatus, Guard } from "src/shared/game/status";
import * as _Status from "src/shared/game/status";
import { showAction } from "src/shared/game/log";
import { Ability } from "src/shared/game/ability";

export function showCrew(
  crew: Crew
) {
  return {...crew,
    actions: crew.actions.map(showAction),
    triggers: crew.triggers.map(showTrigger) };
}

export type Crew = {
  ap: number,
  hp: number,
  maxHp: number,
  ranged: boolean,
  actions: ActionSpec[],
  triggers: Trigger[],
  abilities: Ability[],
};

export function damage<E extends Crew & HasStatus>(
  crew: E,
  damage: number,
) {
  if (crew.Guard === undefined) {
    return focus(crew, over(x => x.hp, x => x - damage));
  } else {
    if (damage <= crew.Guard.guard) {
      return focus(crew, over(x => (<Guard>x.Guard).guard, x => x - damage));
    }

    const leftoverDamage = damage - crew.Guard.guard;
    return focus(crew,
      set(x => x.Guard, undefined),
      over(x => x.hp, x => x - leftoverDamage),
    );
  }
}

export function heal<E extends Crew>(
  crew: E,
  amount: number,
) {
  if (crew.hp + amount > crew.maxHp) {
    return focus(crew, set(x => x.hp, crew.maxHp));
  }
  return focus(crew, set(x => x.hp, crew.hp + amount));
}

export function addHP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew,
    over(x => x.hp, x => x + amount),
    over(x => x.maxHp, x => x + amount),
  );
}

export function addAP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew, over(x => x.ap, x => x + amount));
}

export function damageAP<E extends Crew>(
  crew: E,
  amount: number
) {
  if (amount >= crew.ap) {
    return focus(crew, set(x => x.ap, 0));
  }
  return focus(crew, over(x => x.ap, x => x - amount));
}

export function getAP<C extends Crew>(
  crew: C,
  multiplier: number,
): number {
  return multiplier * (crew.ap + 0); // TODO: add status for increasing damage
}

export function act(
  crew: IdCrew,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  const action = crew.actions[crew.actionIndex];
  state = focus(state,
    over(x => x.crew[index].actionIndex, x => {
      const newX = x + 1;
      return newX >= crew.actions.length ? 0 : newX;
    }),
  );
  return determineAndApplyActionAndTriggers(action, state, log, idGen, crew.id, "ally", { id: crew.id, type: "ally" });
}

const stFighter: Crew = {
  ap: 5,
  hp: 5,
  maxHp: 5,
  triggers: [],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 5,
  }],
  abilities: [
    {
      tag: "SwapSelf",
    }
  ],
};

const stRanged: Crew = {
  ap: 1,
  hp: 1,
  maxHp: 1,
  triggers: [],
  ranged: true,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
  abilities: [],
};

const recruitGrow1: Crew = {
  ap: 1,
  hp: 1,
  maxHp: 1,
  triggers: [
    {
      onTag: "AddCrew",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "Self" },
        value: 1,
      },
      conditions: [],
    },
    {
      onTag: "AddCrew",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
      },
      conditions: [],
    },
  ],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
  abilities: [],
};

const recruitGainAPWhenHP: Crew = {
  ap: 4,
  hp: 2,
  maxHp: 2,
  triggers: [
    {
      onTag: "GainHP",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
      },
      conditions: [],
    },
  ],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
  abilities: [],
};

const recruitKillLast: Crew = {
  ap: 10,
  hp: 10,
  maxHp: 10,
  triggers: [],
  ranged: false,
  actions: [
    /*{
      tag: "Death",
      target: { tag: "Last", type: "ally" }
    },*/
    {
      tag: "Damage",
      target: { tag: "Positions", type: "enemy", positions: [0] },
      value: 10,
    },
  ],
  abilities: [],
};

const abilityHeal: Crew = {
  ap: 10,
  hp: 10,
  maxHp: 10,
  triggers: [],
  ranged: true,
  actions: [
    {
      tag: "Heal",
      target: { tag: "All", type: "ally" },
      value: 1,
    },
  ],
  abilities: [
    {
      tag: "CombinedSpec",
      actions: [
        {
          tag: "Heal",
          target: { tag: "All", type: "ally" },
          value: 4,
        },
        {
          tag: "DeathSelf",
        },
      ]
    }
  ],
};

const armorOnSelfHeal: Crew = {
  ap: 1,
  hp: 5,
  maxHp: 5,
  triggers: [
    {
      onTag: "Heal",
      type: "before",
      action: {
        tag: "AddStatus",
        target: { tag: "Self" },
        status: {
          tag: "Guard",
          value: 1,
          guard: 1,
        },
      },
      conditions: [
        {
          tag: "OwnId",
        }
      ],
    }
  ],
  ranged: false,
  actions: [

  ],
  abilities: [
    {
      tag: "ArmorBash",
      target: { tag: "Positions", type: "enemy", positions: [0] },
    }
  ],
}

const regenOnDamageAlly: Crew = {
  ap: 1,
  hp: 6,
  maxHp: 6,
  triggers: [
    {
      onTag: "Damage",
      type: "before",
      action: {
        tag: "AddStatus",
        target: { tag: "All", type: "ally", },
        status: {
          tag: "Regen",
          value: 1,
        },
      },
      conditions: [
        {
          tag: "TypeCondition",
          type: "ally",
        }
      ],
    }
  ],
  ranged: false,
  actions: [

  ],
  abilities: [
    {
      tag: "AddStatus",
      target: { tag: "All", type: "ally", },
      status: {
        tag: "Guard",
        value: 1,
        guard: 5,
      }
    }
  ],
}

export const allCrew = {
  stFighter: stFighter,
  stRanged: stRanged,
  recruitGrow1: recruitGrow1,
  recruitGainAPWhenHP: recruitGainAPWhenHP,
  abilityHeal: abilityHeal,
  armorOnSelfHeal: armorOnSelfHeal,
  regenOnDamageAlly: regenOnDamageAlly,
};