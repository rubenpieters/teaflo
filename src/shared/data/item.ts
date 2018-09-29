import { Item } from "src/shared/game/item";


export const plus11StartCombat: Item = {
  triggers: [
    /*{
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "GainHP",
        target: { tag: "All", type: "ally" },
        value: 1,
      },
    },
    {
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "GainAP",
        target: { tag: "All", type: "ally" },
        value: 1,
      },
      conditions: [],
    },*/
  ],
};

export const guard1StartCombat: Item = {
  triggers: [
    /*{
      onTag: "AddEnemy",
      type: "before",
      action: {
        tag: "QueueStatus",
        target: { tag: "All", type: "ally" },
        status: {
          tag: "Guard",
          value: 1,
          guard: 1,
        },
      },
    },*/
  ],
};