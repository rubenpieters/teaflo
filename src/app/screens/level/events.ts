import { GameRefs } from "../../states/game";
import { SelectedBuildSchem, currentSchemSol, SelectedExecSchem, schemScholAt, levelData, selectedSchem } from "../act/data";
import { emptyTree, Location } from "../../../shared/tree";
import { updateSolutionRep } from "../exec/events";
import { mkGameState } from "../../../shared/game/state";
import { endStates } from "../../../shared/game/solution";
import { transitionScreen, ScreenSchem } from "../transition";

export function loadLevel(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {
  const sol = schemScholAt(gameRefs, levelId, solId);
  if (sol === undefined || sol.solInfo === undefined) {
    transitionScreen(gameRefs, new ScreenSchem(new SelectedBuildSchem(levelId, solId)));
  } else {
    transitionScreen(gameRefs, new ScreenSchem(new SelectedExecSchem(levelId, solId)));
  }
}

export function newExecLevel(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {
  const sol = currentSchemSol(gameRefs);
  if (sol !== undefined) {
    sol.solInfo = {
      solution: {
        win: false,
        tree: emptyTree(),
      },
      loc: [],
    };
    loadLevel(gameRefs, levelId, solId);
  }
}

/*export function moveCardToFirstFree(
  gameRefs: GameRefs,
  from: { type: "supply" | "deploy", index: number },
  to: { type: "supply" | "deploy" },
) {
  const sol = currentSchemSol(gameRefs);
  if (sol !== undefined) {
    let toIndex: number = sol[to.type].findIndex(x => x === undefined);
    if (toIndex === -1) {
      throw "moveCardToFirstFree, no free slot: should not happen";
    }
    moveCard(gameRefs, from, {...to, ...{ index: toIndex }});
  }
}

export function moveCard(
  gameRefs: GameRefs,
  from: { type: "supply" | "deploy", index: number },
  to: { type: "supply" | "deploy", index: number },
) {
  const sol = currentSchemSol(gameRefs);
  if (sol !== undefined) {
    const oldFrom = sol[from.type][from.index];
    const oldTo = sol[to.type][to.index];
    sol[to.type][to.index] = oldFrom;
    sol[from.type][from.index] = oldTo;

    gameRefs.screens.levelScreen.redrawBox();
  }
}*/

export function toggleDeploy(
  gameRefs: GameRefs,
  supplyIndex: number,
) {
  const sol = currentSchemSol(gameRefs);
  if (sol !== undefined) {
    if (sol.supply[supplyIndex].deployPos === undefined) {
      // deploy this card

      // increase the deployPos for each card
      sol.supply.forEach(x => {
        if (x.deployPos !== undefined) {
          x.deployPos += 1;
        }
      });

      // set newly deployed card to 0
      sol.supply[supplyIndex].deployPos = 0;
    } else {
      // undeploy this card

      // get threshold
      const threshold = sol.supply[supplyIndex].deployPos;

      // unset deployPos
      sol.supply[supplyIndex].deployPos = undefined;

      // decrease all deployPos above threshold
      sol.supply.forEach(x => {
        if (x.deployPos !== undefined && x.deployPos > threshold!) {
          x.deployPos -= 1;
        }
      });
    }

    gameRefs.screens.levelScreen.redrawBox();
  }
}

export function createDeployArray(
  supply: { cardId: string, deployPos: number | undefined }[],
): string[] {
  const sorted = supply.filter(x => x.deployPos !== undefined).sort((x, y) => {
    if (x.deployPos! < y.deployPos!) {
      return -1;
    } else if (x.deployPos! > y.deployPos!) {
      return 1;
    }
    return 0;
  });
  return sorted.map(x => x.cardId);
}

export function levelStats(
  gameRefs: GameRefs,
  levelId: string,
  solIndex: number,
): {
  win: boolean,
} {
  const sol = gameRefs.saveData.act.levels[levelId][solIndex];
  if (sol === undefined || sol.solInfo === undefined) {
    return {
      win: false,
    };
  }

  const frUnits = createDeployArray(sol.supply);
  const enUnits = levelData[levelId].enemyIds;
  const initState = mkGameState(frUnits, enUnits);
  const finalStates = endStates(sol.solInfo.solution.tree, initState);
  const findWin = finalStates.find(x => x.state === "win");
  if (findWin !== undefined) {
    return {
      win: true,
    }
  }
  return {
    win: false,
  }
}