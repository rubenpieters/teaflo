import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";

export type Crew = {
  ap: number,
  hp: number,
  triggers: Trigger[],
}

const stFighter: Crew = {
  ap: 5,
  hp: 5,
  triggers: [],
}

const recruitGrow1: Crew = {
  ap: 1,
  hp: 1,
  triggers: [
    {
      onTag: "Recruit",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "AllCrew" },
        value: 1,
      },
    }
  ]
}

export const allCrew = {
  stFighter: stFighter,
  recruitGrow1: recruitGrow1,
}