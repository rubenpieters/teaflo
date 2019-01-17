import { GameRefs } from "../..//states/game";

export function changeAct(
  gameRefs: GameRefs,
  actId: number,
) {
  gameRefs.saveData.act.selectedActId = actId;
  gameRefs.screens.actScreen.redraw();
}