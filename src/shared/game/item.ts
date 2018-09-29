import { focus, over, set } from "src/shared/iassign-util";
import { Trigger } from "src/shared/game/trigger";

/*export function showItem(
  item: Item
) {
  return {...item,
    triggers: item.triggers.map(t => { return {...t, action: showAction(t.action)}; }) };
}*/

export type Item = {
  triggers: Trigger[],
};