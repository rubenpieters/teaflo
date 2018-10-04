import { focus, over, set } from "src/shared/iassign-util";
import * as allCards from "src/shared/data/card";
import { showSolutionLog } from "src/shared/game/log";
import { createSolCard } from "src/shared/game/ability";
import { addToSolution, fSt } from "test/shared/game/general";

export function mechanic_threat1() {
  let x = addToSolution(allCards.cardRest, { paths: [] }, []);
  x = addToSolution(allCards.cardCrew_0002, x.solution, []);
  x = addToSolution(allCards.cardCrew_0002, x.solution, []);

  const sol = x.result;
  if (sol.state === "invalid") {
    return showSolutionLog + "\n-- INVALID--";
  } else {
    const { state, log } = sol;
    return showSolutionLog(log);
  }
}

console.log(mechanic_threat1());