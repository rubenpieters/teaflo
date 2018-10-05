import { focus, over, set } from "src/shared/iassign-util";
import expect from "expect";
import * as allCards from "src/shared/data/card";
import { showSolutionLog } from "src/shared/game/log";
import { solCardFromAbility } from "src/shared/game/ability";
import { addToSolution, fSt, useAbility } from "test/shared/game/general";

export function mechanic_threat1() {
  let x = addToSolution(allCards.cardRest, { paths: [] }, []);
  x = addToSolution(allCards.cardCrew_0002, x.solution, []);
  x = addToSolution(allCards.cardCrew_0002, x.solution, []);
  x = addToSolution(allCards.cardDummy, x.solution, []);
  x = addToSolution(useAbility(x.result.state, 1, 0), x.solution, [0]);
  x = addToSolution(useAbility(x.result.state, 0, 0), x.solution, [0]);

  const sol = x.result;
  if (sol.state === "invalid") {
    return showSolutionLog + "\n-- INVALID--";
  } else {
    const { state, log } = sol;
    expect(state.crew[0].threatMap[2]).toBe(15);
    expect(state.crew[1].threatMap[2]).toBe(15);
    return showSolutionLog(log);
  }
}

//console.log(mechanic_threat1());
mechanic_threat1();