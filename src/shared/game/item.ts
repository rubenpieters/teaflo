import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";
import { showAction } from "src/shared/game/log";

/*export function showItem(
  item: Item
) {
  return {...item,
    triggers: item.triggers.map(t => { return {...t, action: showAction(t.action)}; }) };
}*/

export type Item = {
  triggers: Trigger[],
};

const plus11StartCombat: Item = {
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

const guard1StartCombat: Item = {
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

export const allItems = {
  plus11StartCombat: plus11StartCombat,
  guard1StartCombat: guard1StartCombat,
};