import { GameRefs } from "../../states/game";
import { mkSolutionData, SelectedActMenu, SelectedLevelMenu } from "./data";

export function changeAct(
  gameRefs: GameRefs,
  actId: number,
) {
  gameRefs.saveData.act.currentMenu = new SelectedActMenu(actId);

  gameRefs.screens.actScreen.actSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawLevelBtn(actId);
}

export function changeLevel(
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.saveData.act.currentMenu = new SelectedLevelMenu(levelId);
  
  gameRefs.screens.actScreen.levelSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawSolBtn(levelId);
}

export function addNewSolution(
  gameRefs: GameRefs,
  levelId: string,
) {
  const solutions = gameRefs.saveData.act.levels[levelId];
  if (solutions === undefined) {
    gameRefs.saveData.act.levels[levelId] = [mkSolutionData()];
  } else {
    gameRefs.saveData.act.levels[levelId].push(mkSolutionData());
  }

  gameRefs.screens.actScreen.redrawSolBtn(levelId);
}