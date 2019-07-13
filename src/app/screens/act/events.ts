import { GameRefs } from "../../states/game";
import { transitionScreen, ScreenAct, ScreenExec } from "../transition";
import { ActDataKeys } from "src/app/data/actData";
import { LevelDataKeys } from "src/app/data/levelData";

export function loadActMenu(
  gameRefs: GameRefs,
) {
  const actId = gameRefs.saveData.currentActId;
  transitionScreen(gameRefs, new ScreenAct(actId));
}

export function changeAct(
  gameRefs: GameRefs,
  actId: ActDataKeys,
) {
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
  transitionScreen(gameRefs, new ScreenExec(levelId));

  /*gameRefs.saveData.act.currentMenu = new SelectedLevelMenu(levelId);
  
  gameRefs.screens.actScreen.levelSelectMode();
  gameRefs.screens.actScreen.redrawActBtn();
  gameRefs.screens.actScreen.drawSolBtn(levelId);
  */
}

export function addNewSolution(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
) {
  /*const solutions = gameRefs.saveData.act.levels[levelId];
  if (solutions === undefined) {
    gameRefs.saveData.act.levels[levelId] = {};
  } else {
    solutions.push(mkSolutionData(levelId));
  }

  gameRefs.screens.actScreen.redrawSolBtn(levelId);*/
}