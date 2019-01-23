import { GameRefs } from "../../states/game";

export function loadActScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(true);
  gameRefs.screens.levelScreen.setVisibility(false);
  // TODO: only activate the correct parts of the act screen
}