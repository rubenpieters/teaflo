import { GameRefs } from "src/app/states/game";
import { currentSolution, currentSchemSol, selectedSchem, levelData } from "../act/data";
import { mkGameState } from "src/shared/game/state";
import { runSolution } from "src/shared/game/solution";

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
}