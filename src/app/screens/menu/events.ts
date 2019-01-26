import { GameRefs } from "../../states/game";
import { selectedSchem, currentSchemSol } from "../act/data";

export function loadActScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(true);
  gameRefs.screens.levelScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(false);
  gameRefs.screens.actScreen.draw();
}