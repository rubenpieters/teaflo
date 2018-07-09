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
        type: "permanent",
      },
    },
    {
      onTag: "StartBattle",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "AllCrew" },
        value: 1,
        type: "permanent",
      },
    },
  ],
};

const guard3StartCombat: Item = {
  triggers: [
    {
      onTag: "StartBattle",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "AllCrew" },
        value: 3,
        type: "temporary",
      },
    },
  ],
}

export const allItems = {
  plus11StartCombat: plus11StartCombat,
  guard3StartCombat: guard3StartCombat,
}