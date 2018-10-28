import { focus, over, set } from "src/shared/iassign-util";
import * as allCards from "src/shared/data/card";
import { showSolutionLog } from "src/shared/game/log";
import { addToSolution, fSt, useAbility } from "./general";
import { emptyTree } from "src/shared/tree";
import expect from "expect";

export function simpleTest() {
  let x = addToSolution(allCards.cardCrew_0002, { solution: emptyTree(), loc: [] }, []);
  x = addToSolution(allCards.cardDummy, x, []);
  x = addToSolution(useAbility(x.result.state, 0, 0), x, [{ tag: "PositionId", type: "enemy", id: 0 }]);

  const sol = x.result;
  if (sol.state === "invalid") {
    return showSolutionLog + "\n-- INVALID--";
  } else {
    const { state, log } = sol;
    expect(state.enemies[0].hp).toBe(985);
    return showSolutionLog(log);
  }
}

//console.log(simpleTest());
simpleTest();