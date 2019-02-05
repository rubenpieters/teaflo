import { GameRefs } from "../../states/game";
import { selectedSchem, currentSchemSol } from "../act/data";
import { updateSolutionRep } from "../exec/events";

export function loadActScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(true);
  gameRefs.screens.levelScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(false);
  gameRefs.screens.actScreen.draw();
}

export function loadExecScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.levelScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(true);
  updateSolutionRep(gameRefs);
}