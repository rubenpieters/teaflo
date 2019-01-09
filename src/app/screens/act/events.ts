import { GameRefs } from "../..//states/game";

export function changeAct(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.redraw();
}