import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";

export type Item = {
  triggers: Trigger[],
}

const plus11StartCombat: Item = {
  triggers: [
    {
      onTag: "StartBattle",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "AllCrew" },
        value: 1,
      },
    },
    {
      onTag: "StartBattle",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "AllCrew" },
        value: 1,
      },
    },
  ]
};

export const allItems = {
  plus11StartCombat: plus11StartCombat,
}