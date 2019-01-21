import { GameRefs } from "../../states/game";
import { mkSolutionData } from "./data";

export function changeAct(
  gameRefs: GameRefs,
  actId: number,
) {
  gameRefs.saveData.act.selectedActId = actId;
  gameRefs.saveData.level.selectedLevelId = undefined;

  gameRefs.screens.actScreen.actSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawLevelBtn(actId);
}

export function changeLevel(
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.saveData.act.selectedActId = undefined;
  gameRefs.saveData.level.selectedLevelId = levelId;
  
  gameRefs.screens.actScreen.levelSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawSolBtn(levelId);
}

export function addNewSolution(
  gameRefs: GameRefs,
  levelId: string,
) {
  const solutions = gameRefs.saveData.level.levels[levelId];
  if (solutions === undefined) {
    gameRefs.saveData.level.levels[levelId] = [mkSolutionData()];
  } else {
    gameRefs.saveData.level.levels[levelId].push(mkSolutionData());
  }

  gameRefs.screens.actScreen.redrawSolBtn(levelId);
}