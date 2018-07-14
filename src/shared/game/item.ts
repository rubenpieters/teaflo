import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";

export type Item = {
  triggers: Trigger[],
};

const plus11StartCombat: Item = {
  triggers: [
    {
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "All", type: "ally" },
        value: 1,
        type: "permanent",
      },
    },
    {
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "All", type: "ally" },
        value: 1,
        type: "permanent",
      },
    },
  ],
};

const guard3StartCombat: Item = {
  triggers: [
    {
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "All", type: "ally" },
        value: 3,
        type: "temporary",
      },
    },
  ],
};

export const allItems = {
  plus11StartCombat: plus11StartCombat,
  guard3StartCombat: guard3StartCombat,
};