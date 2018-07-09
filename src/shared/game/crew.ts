import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";
import { Attack } from "src/shared/game/attack";

export type Crew = {
  ap: number,
  hp: number,
  triggers: Trigger[],
  ranged: boolean,
  attack: Attack,
};

const stFighter: Crew = {
  ap: 5,
  hp: 5,
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
  triggers: [
    {
      onTag: "Recruit",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "Self" },
        value: 1,
      },
    },
    {
      onTag: "Recruit",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "Self" },
        value: 1,
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
  attack: {
    tag: "Damage",
    multiplier: 1,
  },
};

export const allCrew = {
  stFighter: stFighter,
  stRanged: stRanged,
  recruitGrow1: recruitGrow1,
};