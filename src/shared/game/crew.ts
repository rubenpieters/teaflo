import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";
import { Attack } from "src/shared/game/attack";

export type Crew = {
  ap: number,
  hp: number,
  apTemp: number,
  hpTemp: number,
  triggers: Trigger[],
  ranged: boolean,
  attack: Attack,
};

export function damage<C extends Crew>(
  crew: C,
  damage: number,
): C {
 if (damage <= crew.hpTemp) {
   return focus(crew, over(x => x.hpTemp, x => x - damage));
 }

  const leftoverDamage = damage - crew.hpTemp;
  return focus(crew,
    set(x => x.hpTemp, 0),
    over(x => x.hp, x => x - leftoverDamage),
  );
}

export function getAP<C extends Crew>(
  crew: C,
  multiplier: number,
): number {
  return multiplier * (crew.ap + crew.apTemp);
}

export function clearTemp<C extends Crew>(
  crew: C,
): C {
  return focus(crew,
    set(x => x.hpTemp, 0),
    set(x => x.apTemp, 0),
  );
}

const stFighter: Crew = {
  ap: 5,
  hp: 5,
  apTemp: 0,
  hpTemp: 0,
  triggers: [],
  ranged: false,
  attack: {
    tag: "Damage",
    multiplier: 1,
  },
};

const stRanged: Crew = {
  ap: 1,
  hp: 1,
  apTemp: 0,
  hpTemp: 0,
  triggers: [],
  ranged: true,
  attack: {
    tag: "Damage",
    multiplier: 1,
  },
};

const recruitGrow1: Crew = {
  ap: 1,
  hp: 1,
  apTemp: 0,
  hpTemp: 0,
  triggers: [
    {
      onTag: "Recruit",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "Self" },
        value: 1,
        type: "permanent",
      },
    },
    {
      onTag: "Recruit",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
        type: "permanent",
      },
    },
  ],
  ranged: false,
  attack: {
    tag: "Damage",
    multiplier: 1,
  },
};

const recruitGainAPWhenHP: Crew = {
  ap: 4,
  hp: 2,
  apTemp: 0,
  hpTemp: 0,
  triggers: [
    {
      onTag: "GainHP",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
        type: "permanent",
      },
    },
  ],
  ranged: false,
  attack: {
    tag: "Damage",
    multiplier: 1,
  },
};

export const allCrew = {
  stFighter: stFighter,
  stRanged: stRanged,
  recruitGrow1: recruitGrow1,
  recruitGainAPWhenHP: recruitGainAPWhenHP,
};