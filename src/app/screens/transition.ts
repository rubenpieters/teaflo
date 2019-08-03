import { GameRefs } from "../states/game";
import { clearPools, clearAnimations } from "./util";
import { updateSolutionRep } from "./exec/events";
import { CodexTypes } from "./codex/screen";
import { activeScreen } from "../data/saveData";
import { ActDataKeys } from "../data/actData";
import { LevelDataKeys } from "../data/levelData";

export class ScreenAct {
  constructor(
    public readonly actId: ActDataKeys,
    public readonly tag: "ScreenAct" = "ScreenAct",
  ) {}
}

export class ScreenExec {
  constructor(
    public readonly levelId: LevelDataKeys | undefined,
    public readonly tag: "ScreenExec" = "ScreenExec",
  ) {}
}

export class ScreenCodex {
  constructor(
    public readonly page: CodexTypes | undefined = undefined,
    public readonly tag: "ScreenCodex" = "ScreenCodex",
  ) {}
}

export class ScreenSettings {
  constructor(
    public readonly tag: "ScreenSettings" = "ScreenSettings",
  ) {}
}


export type ScreenActive
  = ScreenAct
  | ScreenExec
  | ScreenCodex
  | ScreenSettings
  ;

export function transitionScreen(
  gameRefs: GameRefs,
  newScreen: ScreenActive,
) {
  const oldScreen = activeScreen(gameRefs);
  // clear old screen
  clearAnimations(gameRefs.game, gameRefs.screens.bgScreen);
  clearPools(gameRefs.screens.menuScreen);
  switch (oldScreen.tag) {
    case "ScreenAct": {
      clearAnimations(gameRefs.game, gameRefs.screens.actScreen);
      clearPools(gameRefs.screens.actScreen);
      break;
    }
    case "ScreenExec": {
      clearAnimations(gameRefs.game, gameRefs.screens.execScreen);
      clearPools(gameRefs.screens.execScreen);
      break;
    }
    case "ScreenCodex": {
      clearAnimations(gameRefs.game, gameRefs.screens.codexScreen);
      clearPools(gameRefs.screens.codexScreen);
      break;
    }
    case "ScreenSettings": {
      clearAnimations(gameRefs.game, gameRefs.screens.settingsScreen);
      clearPools(gameRefs.screens.settingsScreen);
      break;
    }
  }
  if (newScreen.tag !== oldScreen.tag) {
    gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => drawNewScreen(gameRefs, newScreen, true)
    );
  } else {
    drawNewScreen(gameRefs, newScreen, false);
  }
}

function drawNewScreen(
  gameRefs: GameRefs,
  newScreen: ScreenActive,
  animations: boolean,
) {
  gameRefs.screens.menuScreen.drawMenuBtn(animations);
  gameRefs.screens.menuScreen.drawBg();
  switch (newScreen.tag) {
    case "ScreenAct": {
      gameRefs.saveData.activeScreen = "menu";
      const actId = newScreen.actId;
      gameRefs.saveData.currentActId = actId;
      gameRefs.screens.actScreen.actSelectMode();
      gameRefs.screens.actScreen.drawActBtn(animations);
      gameRefs.screens.actScreen.drawBg(animations);
      gameRefs.screens.actScreen.drawLevelBtn(actId);
      break;
    }
    case "ScreenExec": {
      gameRefs.saveData.activeScreen = "exec";
      const levelId = newScreen.levelId;
      gameRefs.saveData.currentLevelId = levelId;
      gameRefs.screens.execScreen.reset();
      updateSolutionRep(gameRefs);
      break;
    }
    case "ScreenCodex": {
      gameRefs.saveData.activeScreen = "codex";
      if (newScreen.page !== undefined) {
        gameRefs.screens.codexScreen.page = newScreen.page;
      }
      gameRefs.screens.codexScreen.reset();
      gameRefs.screens.codexScreen.drawPage();
      break;
    }
    case "ScreenSettings": {
      gameRefs.saveData.activeScreen = "settings";
      gameRefs.screens.settingsScreen.drawBtn();
      break;
    }
  }
}