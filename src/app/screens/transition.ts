import { SelectedBuildSchem, SelectedExecSchem, SelectedActMenu, SelectedLevelMenu, currentScreen } from "./act/data";
import { GameRefs } from "../states/game";
import { clearPools } from "./util";
import { updateSolutionRep } from "./exec/events";
import { CodexTypes } from "./codex/screen";

export class ScreenAct {
  constructor(
    public readonly selected: SelectedActMenu | SelectedLevelMenu | undefined,
    public readonly tag: "ScreenAct" = "ScreenAct",
  ) {}
}

export class ScreenSchem {
  constructor(
    public readonly selected: SelectedBuildSchem | SelectedExecSchem | undefined,
    public readonly tag: "ScreenSchem" = "ScreenSchem",
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
  | ScreenSchem
  | ScreenCodex
  | ScreenSettings
  ;

export function transitionScreen(
  gameRefs: GameRefs,
  newScreen: ScreenActive,
) {
  const oldScreen = currentScreen(gameRefs);
  // clear old screen
  clearPools(gameRefs.screens.menuScreen);
  gameRefs.screens.bgScreen.clearAnimations();
  switch (oldScreen.tag) {
    case "ScreenAct": {
      gameRefs.screens.actScreen.clearAnimations();
      clearPools(gameRefs.screens.actScreen);
      break;
    }
    case "ScreenSchem": {
      gameRefs.screens.execScreen.clearAnimations();
      clearPools(gameRefs.screens.levelScreen);
      clearPools(gameRefs.screens.execScreen);
      break;
    }
    case "ScreenCodex": {
      clearPools(gameRefs.screens.codexScreen);
      break;
    }
    case "ScreenSettings": {
      clearPools(gameRefs.screens.settingsScreen);
      break;
    }
  }
  gameRefs.screens.bgScreen.bgOnIntroComplete(
    () => {
      // draw new screen
      gameRefs.screens.menuScreen.drawMenuBtn();
      switch (newScreen.tag) {
        case "ScreenAct": {
          gameRefs.saveData.act.activeScreen = "menu";
          if (newScreen.selected !== undefined) {
            switch (newScreen.selected.tag) {
              case "SelectedActMenu": {
                const actId = newScreen.selected.actId;
                gameRefs.saveData.act.currentMenu = new SelectedActMenu(actId);
                gameRefs.screens.actScreen.actSelectMode();
                gameRefs.screens.actScreen.drawActBtn();
                gameRefs.screens.actScreen.drawLevelBtn(actId);
                break;
              }
              case "SelectedLevelMenu": {
                const levelId = newScreen.selected.levelId;
                gameRefs.saveData.act.currentMenu = new SelectedLevelMenu(levelId);
                gameRefs.screens.actScreen.levelSelectMode();
                gameRefs.screens.actScreen.drawActBtn();
                gameRefs.screens.actScreen.drawSolBtn(levelId);
                break;
              }
            }
          } else {
            gameRefs.saveData.act.currentMenu = undefined;
            gameRefs.screens.actScreen.drawActBtn();
          }
          break;
        }
        case "ScreenSchem": {
          gameRefs.saveData.act.activeScreen = "schem";
          if (newScreen.selected !== undefined) {
            switch (newScreen.selected.tag) {
              case "SelectedBuildSchem": {
                const levelId = newScreen.selected.levelId;
                const solId = newScreen.selected.solId;
                gameRefs.saveData.act.currentSchem = new SelectedBuildSchem(levelId, solId);
                gameRefs.screens.levelScreen.drawBox();
                break;
              }
              case "SelectedExecSchem": {
                const levelId = newScreen.selected.levelId;
                const solId = newScreen.selected.solId;
                gameRefs.saveData.act.currentSchem = new SelectedExecSchem(levelId, solId);
                gameRefs.screens.execScreen.reset();
                updateSolutionRep(gameRefs);
                break;
              }
            }
          }
          break;
        }
        case "ScreenCodex": {
          gameRefs.saveData.act.activeScreen = "codex";
          if (newScreen.page !== undefined) {
            gameRefs.screens.codexScreen.page = newScreen.page;
          }
          gameRefs.screens.codexScreen.drawPage();
          break;
        }
        case "ScreenSettings": {
          gameRefs.saveData.act.activeScreen = "settings";
          break;
        }
      }
    }
  );
}