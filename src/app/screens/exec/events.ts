import { GameRefs } from "../../../app/states/game";
import { currentSolution, currentSchemSol, selectedSchem, levelData, setSolution } from "../act/data";
import { mkGameState } from "../../../shared/game/state";
import { runSolution, extendSolution, SolutionData } from "../../../shared/game/solution";
import { GlobalId, eqUnitId } from "../../../shared/game/entityId";

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

  gameRefs.screens.execScreen.state = solResult.state;
  gameRefs.screens.execScreen.drawFriendlyUnits();
  gameRefs.screens.execScreen.drawStats();
}

export function hoverUnit(
  gameRefs: GameRefs,
  globalId: GlobalId<"friendly" | "enemy">,
) {
  // TODO: only redraw if necessary?
  gameRefs.screens.execScreen.hoveredUnit = globalId;
  gameRefs.screens.execScreen.drawStats();
}

export function clearHover(
  gameRefs: GameRefs,
) {
  gameRefs.screens.execScreen.hoveredUnit = undefined;
  gameRefs.screens.execScreen.drawStats();
}

export function clickUnit(
  gameRefs: GameRefs,
  globalId: GlobalId<"friendly" | "enemy">,
) {
  const selected = gameRefs.screens.execScreen.selectedUnit;
  if (selected !== undefined && eqUnitId(gameRefs.screens.execScreen.state!, globalId, selected)) {
    gameRefs.screens.execScreen.selectedUnit = undefined;
  } else {
    gameRefs.screens.execScreen.selectedUnit = globalId;
  }
  gameRefs.screens.execScreen.drawStats();
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