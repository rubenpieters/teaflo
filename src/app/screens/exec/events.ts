import { GameRefs } from "../../../app/states/game";
import { runSolution, extendSolution, SolutionData, cutSolution, emptySolution } from "../../../shared/game/solution";
import { Location, getLocation, emptyTree } from "../../../shared/tree";
import { firstLogIndex } from "../../../shared/game/log";
import { runAsTween } from "../../phaser/animation";
import { clearAnimations } from "../util";
import { mkGameState } from "../../../shared/game/state";
import { TargetId } from "../../../shared/definitions/entityId";
import { FrUnitId } from "../../../shared/data/frUnitMap";
import { selectedLevelId, getLevelSaveDataAndFillDefault, compositionToKey, validComposition, setLocation, currentSolution, setSolution } from "../../data/saveData";
import { levelData } from "../../data/levelData";

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
  const levelId = selectedLevelId(gameRefs);
  if (levelId === undefined) {
    return;
  }

  const saveData = getLevelSaveDataAndFillDefault(gameRefs, levelId);

  const composition = saveData.currentComposition;
  const solMap = saveData.solMap;
  let solData = solMap[compositionToKey(composition)];

  const frUnits = composition;
  const enUnits = levelData[levelId].enemyIds;
  const initState = mkGameState(frUnits, enUnits);

  if (! validComposition(composition, levelId)) {
    gameRefs.screens.execScreen.state = initState;

    gameRefs.screens.execScreen.drawState(initState);
    gameRefs.screens.execScreen.drawStats(initState);
    gameRefs.screens.execScreen.drawBgSprites();
    gameRefs.screens.execScreen.drawAnimControlBtns();
    gameRefs.screens.execScreen.drawTreeControlBtns();
    gameRefs.screens.execScreen.drawSwitchOrderBtns();
    gameRefs.screens.execScreen.drawClearBtn();
    gameRefs.screens.execScreen.clearTree();
    gameRefs.screens.execScreen.logActionPool.clear();
  } else if (solData === undefined || solData.solInfo.loc.length === 0) {
    if (solData === undefined) {
      solData = {
        composition,
        solInfo: {
          solution: emptySolution,
          loc: [],
        },
      };
      solMap[compositionToKey(composition)] = solData;
    }
    gameRefs.screens.execScreen.state = initState;

    gameRefs.screens.execScreen.drawState(initState);
    gameRefs.screens.execScreen.drawStats(initState);
    gameRefs.screens.execScreen.drawBgSprites();
    gameRefs.screens.execScreen.drawAnimControlBtns();
    gameRefs.screens.execScreen.drawTreeControlBtns();
    gameRefs.screens.execScreen.drawSwitchOrderBtns();
    gameRefs.screens.execScreen.drawClearBtn();
    gameRefs.screens.execScreen.drawTree(solData.solInfo);
    gameRefs.screens.execScreen.logActionPool.clear();
  } else {
    const solResult = runSolution(solData.solInfo.solution, solData.solInfo.loc, initState);
    const prevState = gameRefs.screens.execScreen.state;
    gameRefs.screens.execScreen.state = solResult.state;
    gameRefs.screens.execScreen.log = solResult.log;
    // update tree rep
    clearAnimations(gameRefs.game, gameRefs.screens.execScreen);
    gameRefs.screens.execScreen.clearAnimPools();
    gameRefs.screens.execScreen.drawTree(solData.solInfo);
    if (prevState === undefined) {
      gameRefs.screens.execScreen.drawBgSprites();
      gameRefs.screens.execScreen.drawAnimControlBtns();
      gameRefs.screens.execScreen.drawTreeControlBtns();
      gameRefs.screens.execScreen.drawSwitchOrderBtns();
      gameRefs.screens.execScreen.drawClearBtn();
    }
    const logIndex = firstLogIndex();
    if (logIndex !== undefined) {
      const solutionData = getLocation(solData.solInfo.solution.tree, solData.solInfo.loc);
      // TODO: pass prevState to intermediateActions, instead of having to find it somehow
      const anim = gameRefs.screens.execScreen.drawIntermediateActions(
        solResult.state,
        solResult.prevState,
        solutionData.ability,
        solutionData.origin,
        solResult.log,
      );
      runAsTween(gameRefs, anim, "log");
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
  const newSol = extendSolution(solData, solInfo.solution, solInfo.loc);
  setSolution(gameRefs, newSol);

  updateSolutionRep(gameRefs);
  gameRefs.screens.execScreen.clickState = undefined;
  gameRefs.screens.execScreen.intermediate = undefined;
}

export function changeLevelLoc(
  gameRefs: GameRefs,
  loc: Location,
) {
  setLocation(gameRefs, loc);

  updateSolutionRep(gameRefs);
  gameRefs.screens.execScreen.clickState = undefined;
  gameRefs.screens.execScreen.intermediate = undefined;
}

export function cutLevelSolution(
  gameRefs: GameRefs,
  loc: Location,
) {
  const solInfo = currentSolution(gameRefs);
  const newSolution = cutSolution(solInfo.solution, loc);
  setSolution(gameRefs, newSolution);

  updateSolutionRep(gameRefs);
  gameRefs.screens.execScreen.clickState = undefined;
  gameRefs.screens.execScreen.intermediate = undefined;
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
  const levelId = selectedLevelId(gameRefs);
  gameRefs.saveData.levelSaves[levelId!]!.currentComposition[position] = cardId;
  updateSolutionRep(gameRefs);
  hideChangeUnitScreen(gameRefs);
}