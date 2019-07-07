import { GameRefs } from "../../../app/states/game";
import { currentSolution, currentSolMap, levelData, setSolution, setLocation, selectedLevelId, initSol, emptyComposition, validComposition, selectedSchemLevelId, currentSol, compositionToKey, selectedSchemComposition } from "../act/data";
import { runSolution, extendSolution, SolutionData, cutSolution } from "../../../shared/game/solution";
import { Location } from "../../../shared/tree";
import { firstLogIndex } from "../../../shared/game/log";
import { runAsTween } from "../../phaser/animation";
import { clearAnimations } from "../util";
import { mkGameState } from "../../../shared/game/state";
import { TargetId } from "../../../shared/definitions/entityId";
import deepEqual from "deep-equal";
import { CardId } from "src/shared/data/cardId";
import { FrUnitId } from "src/shared/data/frUnitMap";

export function drawCurrentLevel(
  gameRefs: GameRefs,
) {
  const levelId = selectedLevelId(gameRefs);
  if (levelId !== undefined) {
    // loadLevel(gameRefs, levelId);
  }
}

export function updateSolutionRep(
  gameRefs: GameRefs,
) {
  let sol = currentSol(gameRefs);
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId === undefined) {
    return;
  }

  if (sol === undefined) {
    sol = initSol(gameRefs, levelId);
  }
  
  const composition = sol.currentComposition;
  const solMap = sol.solMap;
  const solData = solMap[compositionToKey(composition)];

  const frUnits = composition;
  const enUnits = levelData[levelId].enemyIds;
  const initState = mkGameState(frUnits, enUnits);

  if (solData === undefined || ! validComposition(composition, levelId)) {
    gameRefs.screens.execScreen.state = initState;

    gameRefs.screens.execScreen.drawState(initState);
    gameRefs.screens.execScreen.drawStats(initState);
    gameRefs.screens.execScreen.drawAnimControlBtns();
    gameRefs.screens.execScreen.drawTreeControlBtns();
    gameRefs.screens.execScreen.drawSwitchOrderBtns();
    gameRefs.screens.execScreen.drawClearBtn();
  } else {
    const solResult = runSolution(solData.solInfo.solution, solData.solInfo.loc, initState);

    const prevState = gameRefs.screens.execScreen.state;
    gameRefs.screens.execScreen.state = solResult.state;
    gameRefs.screens.execScreen.log = solResult.log;
    // update tree rep
    clearAnimations(gameRefs.game, gameRefs.screens.execScreen);
    gameRefs.screens.execScreen.clearAnimPools();
    gameRefs.screens.execScreen.drawTree(solData.solInfo!);
    if (prevState === undefined) {
      gameRefs.screens.execScreen.drawAnimControlBtns();
      gameRefs.screens.execScreen.drawTreeControlBtns();
      gameRefs.screens.execScreen.drawSwitchOrderBtns();
      gameRefs.screens.execScreen.drawClearBtn();
    }
    const logIndex = firstLogIndex();
    if (logIndex !== undefined) {
      runAsTween(gameRefs, gameRefs.screens.execScreen.drawIntermediateActions(solResult.state, solResult.log), "log");
    } else {
      gameRefs.screens.execScreen.drawState(solResult.state);
      gameRefs.screens.execScreen.drawStats(solResult.state);
    }
  }
}

export function hoverUnit(
  gameRefs: GameRefs,
  globalId: TargetId,
) {
  // TODO: only redraw if necessary?
  gameRefs.screens.execScreen.hoveredUnit = globalId;
  gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
}

export function clearHover(
  gameRefs: GameRefs,
) {
  gameRefs.screens.execScreen.hoveredUnit = undefined;
  gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.currentState());
}

export function extendLevelSolution(
  gameRefs: GameRefs,
  solData: SolutionData,
) {
  const solInfo = currentSolution(gameRefs);
  if (solInfo !== undefined) {
    const newSol = extendSolution(solData, solInfo.solution, solInfo.loc);
    setSolution(gameRefs, newSol);

    updateSolutionRep(gameRefs);
    gameRefs.screens.execScreen.clickState = undefined;
    gameRefs.screens.execScreen.intermediate = undefined;
  }
}

export function changeLevelLoc(
  gameRefs: GameRefs,
  loc: Location,
) {
  const solInfo = currentSolution(gameRefs);
  if (solInfo !== undefined) {
    setLocation(gameRefs, loc);

    updateSolutionRep(gameRefs);
    gameRefs.screens.execScreen.clickState = undefined;
    gameRefs.screens.execScreen.intermediate = undefined;
  }
}

export function cutLevelSolution(
  gameRefs: GameRefs,
  loc: Location,
) {
  const solInfo = currentSolution(gameRefs);
  if (solInfo !== undefined) {
    const newSolution = cutSolution(solInfo.solution, loc);
    setSolution(gameRefs, newSolution);

    updateSolutionRep(gameRefs);
    gameRefs.screens.execScreen.clickState = undefined;
    gameRefs.screens.execScreen.intermediate = undefined;
  }
}

/*
export function clearSolution(
  gameRefs: GameRefs,
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    gameRefs.saveData.act.levels[schem.levelId]![schem.solId].supply.forEach(x => {
      x.deployPos = undefined;
    });
    gameRefs.saveData.act.levels[schem.levelId]![schem.solId].solInfo = undefined;
  }
  drawCurrentLevel(gameRefs);
}
*/

export function showChangeUnitScreen(
  gameRefs: GameRefs,
  position: number,
) {
  gameRefs.screens.execScreen.selecting = position;
  gameRefs.screens.execScreen.drawUnitSelect();
}

export function hideChangeUnitScreen(
  gameRefs: GameRefs,
) {
  gameRefs.screens.execScreen.selecting = undefined;
  gameRefs.screens.execScreen.drawUnitSelect();
}

export function deployUnit(
  gameRefs: GameRefs,
  cardId: FrUnitId,
  position: number,
) {
  const levelId = selectedSchemLevelId(gameRefs);
  gameRefs.saveData.act.levels[levelId!]!.currentComposition[position] = cardId;
  updateSolutionRep(gameRefs);
  hideChangeUnitScreen(gameRefs);
}