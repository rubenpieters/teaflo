import { GameRefs } from "../../states/game";
import { SelectedBuildSchem, currentSchemSol } from "../act/data";


export function loadLevel(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {
  gameRefs.saveData.act.currentSchem = new SelectedBuildSchem(levelId, solId);

  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.levelScreen.setVisibility(true);

  gameRefs.screens.levelScreen.drawBox();
}

export function moveCardToFirstFree(
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
}