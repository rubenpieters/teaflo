import { GameRefs } from "../../states/game";

export function loadActScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(true);
  gameRefs.screens.levelScreen.setVisibility(false);
  gameRefs.screens.actScreen.draw();
}