import { SelectedActMenu, SelectedLevelMenu, currentScreen, LevelDataKeys, emptyComposition } from "./act/data";
import { GameRefs } from "../states/game";
import { clearPools, clearAnimations } from "./util";
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
    public readonly levelId: LevelDataKeys | undefined,
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
  clearAnimations(gameRefs.game, gameRefs.screens.bgScreen);
  clearPools(gameRefs.screens.menuScreen);
  switch (oldScreen.tag) {
    case "ScreenAct": {
      clearAnimations(gameRefs.game, gameRefs.screens.actScreen);
      clearPools(gameRefs.screens.actScreen);
      break;
    }
    case "ScreenSchem": {
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
          const levelId = newScreen.levelId;
          gameRefs.saveData.act.currentLevelId = levelId;
          gameRefs.screens.execScreen.reset();
          updateSolutionRep(gameRefs);
          break;
        }
        case "ScreenCodex": {
          gameRefs.saveData.act.activeScreen = "codex";
          if (newScreen.page !== undefined) {
            gameRefs.screens.codexScreen.page = newScreen.page;
          }
          gameRefs.screens.codexScreen.reset();
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