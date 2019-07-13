import { GameRefs } from "../../states/game";
import { updateSolutionRep } from "../exec/events";

export function loadActScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(true);
  gameRefs.screens.execScreen.setVisibility(false);
  gameRefs.screens.codexScreen.setVisibility(false);
  gameRefs.screens.settingsScreen.setVisibility(false);
  gameRefs.saveData.activeScreen = "menu";
  gameRefs.screens.actScreen.draw();
  gameRefs.screens.menuScreen.redrawMenuBtn();
}

export function loadExecScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(true);
  gameRefs.screens.codexScreen.setVisibility(false);
  gameRefs.screens.settingsScreen.setVisibility(false);
  gameRefs.saveData.activeScreen = "exec";
  updateSolutionRep(gameRefs);
  gameRefs.screens.menuScreen.redrawMenuBtn();
}

export function loadCodexScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(false);
  gameRefs.screens.codexScreen.setVisibility(true);
  gameRefs.screens.settingsScreen.setVisibility(false);
  gameRefs.saveData.activeScreen = "codex";
  gameRefs.screens.codexScreen.drawPage();
  gameRefs.screens.menuScreen.redrawMenuBtn();
}

export function loadSettingsScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.execScreen.setVisibility(false);
  gameRefs.screens.codexScreen.setVisibility(false);
  gameRefs.screens.settingsScreen.setVisibility(true);
  gameRefs.saveData.activeScreen = "settings";
  gameRefs.screens.menuScreen.redrawMenuBtn();
}