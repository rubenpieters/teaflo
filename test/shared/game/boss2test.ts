import { focus, over, set } from "src/shared/iassign-util";
import * as allCards from "src/shared/data/card";
import { showSolutionLog } from "src/shared/game/log";
import { addToSolution, fSt, useAbility } from "./general";
import { emptyTree } from "src/shared/tree";
import expect from "expect";

export function boss2Test() {
  let x = addToSolution(allCards.cardCrew_0004, { solution: emptyTree(), loc: [] }, []);
  x = addToSolution(allCards.cardCrew_0005, x, []);
  x = addToSolution(allCards.cardEnemy_0002, x, []);

  const sol = x.result;
  if (sol.state === "invalid") {
    return showSolutionLog + "\n-- INVALID--";
  } else {
    const { state, log } = sol;
    expect(state.enemies[0].hp).toBe(985);
    return showSolutionLog(log);
  }
}

console.log(boss2Test());
//boss2Test();