import { GameRefs } from "../../../app/states/game";
import { currentSolution, currentSchemSol, selectedSchem, levelData, setSolution, setLocation } from "../act/data";
import { mkGameState } from "../../../shared/game/state";
import { runSolution, extendSolution, SolutionData } from "../../../shared/game/solution";
import { GlobalId, eqUnitId } from "../../../shared/game/entityId";
import { UnitSelection } from "./screen";
import { loadLevel } from "../level/events";
import { Location } from "src/shared/tree";

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
    gameRefs.screens.execScreen.drawAnimControlBtns();
  }
  const firstLogKey = gameRefs.screens.execScreen.firstLogKey();
  if (firstLogKey !== undefined) {
    gameRefs.screens.execScreen.drawIntermediateLog(firstLogKey, true);
  } else {
    gameRefs.screens.execScreen.drawState(solResult.state);
    gameRefs.screens.execScreen.drawStats(solResult.state);
  }
}

export function hoverUnit(
  gameRefs: GameRefs,
  globalId: UnitSelection,
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

export function clickUnit(
  gameRefs: GameRefs,
  globalId: UnitSelection,
) {
  const selected = gameRefs.screens.execScreen.selectedUnit;
  if (selected !== undefined && eqUnitId(gameRefs.screens.execScreen.currentState(), globalId, selected)) {
    gameRefs.screens.execScreen.selectedUnit = undefined;
  } else {
    gameRefs.screens.execScreen.selectedUnit = globalId;
  }
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