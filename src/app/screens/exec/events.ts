import { GameRefs } from "../../../app/states/game";
import { currentSolution, currentSchemSol, selectedSchem, levelData, setSolution } from "../act/data";
import { mkGameState } from "../../../shared/game/state";
import { runSolution, extendSolution, SolutionData } from "../../../shared/game/solution";
import { GlobalId, eqUnitId } from "../../../shared/game/entityId";
import { UnitSelection } from "./screen";
import { loadLevel } from "../level/events";

export function drawCurrentLevel(
  gameRefs: GameRefs,
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    loadLevel(gameRefs, schem.levelId, schem.solId);
  }
}

export function updateSolutionRep(
  gameRefs: GameRefs,
) {
  const sol = currentSchemSol(gameRefs);
  const schem = selectedSchem(gameRefs);
  if (schem === undefined || sol === undefined || sol.solInfo === undefined) {
    return;
  }

  const frUnits = sol.deploy;
  const enUnits = levelData[schem.levelId].enemyIds;
  const initState = mkGameState(frUnits, enUnits);
  const solResult = runSolution(sol.solInfo.solution, sol.solInfo.loc, initState);

  const prevState = gameRefs.screens.execScreen.state;
  gameRefs.screens.execScreen.state = solResult.state;
  gameRefs.screens.execScreen.log = solResult.log;
  // update tree rep
  gameRefs.screens.execScreen.drawTree(sol.solInfo!);
  if (prevState === undefined) {
    // there was no previous state, just draw
    gameRefs.screens.execScreen.drawState(solResult.state);
    gameRefs.screens.execScreen.drawStats(solResult.state);
    gameRefs.screens.execScreen.drawAnimControlBtns();
  } else {
    // draw log animations
    gameRefs.screens.execScreen.drawLogAnimation(prevState);
  }
}

export function hoverUnit(
  gameRefs: GameRefs,
  globalId: UnitSelection,
) {
  // TODO: only redraw if necessary?
  gameRefs.screens.execScreen.hoveredUnit = globalId;
  gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
}

export function clearHover(
  gameRefs: GameRefs,
) {
  gameRefs.screens.execScreen.hoveredUnit = undefined;
  gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
}

export function clickUnit(
  gameRefs: GameRefs,
  globalId: UnitSelection,
) {
  const selected = gameRefs.screens.execScreen.selectedUnit;
  if (selected !== undefined && eqUnitId(gameRefs.screens.execScreen.state!, globalId, selected)) {
    gameRefs.screens.execScreen.selectedUnit = undefined;
  } else {
    gameRefs.screens.execScreen.selectedUnit = globalId;
  }
  gameRefs.screens.execScreen.drawStats(gameRefs.screens.execScreen.state!);
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
  }
}