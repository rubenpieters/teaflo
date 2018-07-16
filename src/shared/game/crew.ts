import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";
import { GameState, IdCrew } from "src/shared/game/state";
import { ActionTarget, ActionSpec, determineAndApplyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";

export type Crew = {
  ap: number,
  hp: number,
  triggers: Trigger[],
  ranged: boolean,
  actions: ActionSpec[],
};

export function damage<E extends Crew>(
  crew: E,
  damage: number,
) {
  return focus(crew,
    over(x => x.hp, x => x - damage),
  );
}

/*export function damage<E extends Crew>(
  crew: E,
  damage: number,
): E {
 if (damage <= crew.hpTemp) {
   return focus(crew, over(x => x.hpTemp, x => x - damage));
 }

  const leftoverDamage = damage - crew.hpTemp;
  return focus(crew,
    set(x => x.hpTemp, 0),
    over(x => x.hp, x => x - leftoverDamage),
  );
}*/

export function addHP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew, over(x => x.hp, x => x + amount));
}

export function addAP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew, over(x => x.ap, x => x + amount));
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
  return determineAndApplyActionAndTriggers(action, state, log, idGen, crew.id, "ally");
}

const stFighter: Crew = {
  ap: 5,
  hp: 5,
  triggers: [],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 5,
  }],
};

const stRanged: Crew = {
  ap: 1,
  hp: 1,
  triggers: [],
  ranged: true,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
};

const recruitGrow1: Crew = {
  ap: 1,
  hp: 1,
  triggers: [
    {
      onTag: "AddCrew",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "Self" },
        value: 1,
      },
    },
    {
      onTag: "AddCrew",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
      },
    },
  ],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
};

const recruitGainAPWhenHP: Crew = {
  ap: 4,
  hp: 2,
  triggers: [
    {
      onTag: "GainHP",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
      },
    },
  ],
  ranged: false,
  actions: [{
    tag: "Damage",
    target: { tag: "Positions", type: "enemy", positions: [0] },
    value: 1,
  }],
};

const recruitKillLast: Crew = {
  ap: 10,
  hp: 10,
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
};

export const allCrew = {
  stFighter: stFighter,
  stRanged: stRanged,
  recruitGrow1: recruitGrow1,
  recruitGainAPWhenHP: recruitGainAPWhenHP,
};