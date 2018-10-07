import { focus, over, set } from "src/shared/iassign-util";
import * as allCards from "src/shared/data/card";
import { showSolutionLog } from "src/shared/game/log";
import { addToSolution, fSt } from "./general";

/*
export function level3Test() {
  let x = addToSolution(allCards.cardRest, { paths: [] }, []);
  x = addToSolution(allCards.cardCrew_0009, x.solution, []);
  x = addToSolution(allCards.cardBattle_0012, x.solution, []);
  x = addToSolution(createSolCard(
    fSt(x.result.state).crew[0].abilities[1].effect([0])(fSt(x.result.state), { tag: "PositionId", id: 0, type: "ally", }),
    0, "ally"
  ), x.solution);
  x = addToSolution(createSolCard(
    fSt(x.result.state).crew[0].abilities[0].effect([])(fSt(x.result.state), { tag: "PositionId", id: 0, type: "ally", }),
    0, "ally"
  ), x.solution);
  x = addToSolution(createSolCard(
    fSt(x.result.state).crew[0].abilities[1].effect([0])(fSt(x.result.state), { tag: "PositionId", id: 0, type: "ally", }),
    0, "ally"
  ), x.solution);
  x = addToSolution(createSolCard(
    fSt(x.result.state).crew[0].abilities[0].effect([])(fSt(x.result.state), { tag: "PositionId", id: 0, type: "ally", }),
    0, "ally"
  ), x.solution);
  x = addToSolution(createSolCard(
    fSt(x.result.state).crew[0].abilities[1].effect([0])(fSt(x.result.state), { tag: "PositionId", id: 0, type: "ally", }),
    0, "ally"
  ), x.solution);


  const sol = x.result;
  if (sol.state === "invalid") {
    return showSolutionLog + "\n-- INVALID--";
  } else {
    const { state, log } = sol;
    return showSolutionLog(log);
  }
}
*/

// console.log(level3Test());