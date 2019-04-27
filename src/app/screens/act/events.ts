import { GameRefs } from "../../states/game";
import { mkSolutionData, SelectedActMenu, SelectedLevelMenu, LevelDataKeys } from "./data";
import { transitionScreen, ScreenAct } from "../transition";

export function loadActMenu(
  gameRefs: GameRefs,
) {
  const menu = gameRefs.saveData.act.currentMenu;
  if (menu !== undefined) {
    switch (menu.tag) {
      case "SelectedActMenu": {
        transitionScreen(gameRefs, new ScreenAct(menu));
        break;
      }
      case "SelectedLevelMenu": {
        transitionScreen(gameRefs, new ScreenAct(menu));
        break;
      }
    }
  } else {
    transitionScreen(gameRefs, new ScreenAct(undefined));
  }
}

export function changeAct(
  gameRefs: GameRefs,
  actId: number,
) {
  gameRefs.saveData.act.currentMenu = new SelectedActMenu(actId);

  gameRefs.screens.actScreen.clearAnimations();
  gameRefs.screens.bgScreen.clearAnimations();
  gameRefs.screens.actScreen.actSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawLevelBtn(actId);
}

export function changeLevel(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
) {
  gameRefs.saveData.act.currentMenu = new SelectedLevelMenu(levelId);
  
  gameRefs.screens.actScreen.levelSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawSolBtn(levelId);
}

export function addNewSolution(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
) {
  const solutions = gameRefs.saveData.act.levels[levelId];
  if (solutions === undefined) {
    gameRefs.saveData.act.levels[levelId] = [mkSolutionData(levelId)];
  } else {
    solutions.push(mkSolutionData(levelId));
  }

  gameRefs.screens.actScreen.redrawSolBtn(levelId);
}