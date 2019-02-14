import { SelectedBuildSchem, SelectedExecSchem, SelectedActMenu, SelectedLevelMenu, currentScreen } from "./act/data";
import { GameRefs } from "../states/game";
import { clearPools } from "./util";

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
          const f = () => {
            if (oldScreen.tag === newScreen.tag) {
              // without animations
              gameRefs.screens.actScreen.redrawActBtn();
            } else {
              // with animations
              gameRefs.screens.actScreen.drawActBtn();
            }
          }
          if (newScreen.selected !== undefined) {
            switch (newScreen.selected.tag) {
              case "SelectedActMenu": {
                const actId = newScreen.selected.actId;
                gameRefs.saveData.act.currentMenu = new SelectedActMenu(actId);
                gameRefs.screens.actScreen.actSelectMode();
                f();
                gameRefs.screens.actScreen.drawLevelBtn(actId);
                break;
              }
              case "SelectedLevelMenu": {
                const levelId = newScreen.selected.levelId;
                gameRefs.saveData.act.currentMenu = new SelectedLevelMenu(levelId);
                gameRefs.screens.actScreen.levelSelectMode();
                f();
                gameRefs.screens.actScreen.drawSolBtn(levelId);
                break;
              }
            }
          }
          break;
        }
        case "ScreenSchem": {
          break;
        }
        case "ScreenCodex": {
          break;
        }
        case "ScreenSettings": {
          break;
        }
      }
    }
  );
}