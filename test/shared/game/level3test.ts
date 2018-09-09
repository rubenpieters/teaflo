import { focus, over, set } from "src/shared/iassign-util";
import { allCards} from "src/shared/game/card";
import { showSolutionLog } from "src/shared/game/log";
import { createSolCard } from "src/shared/game/ability";
import { addToSolution, passNoInputsR, passNoInputsE, fSt } from "./general";

export function level3Test() {
  let x = addToSolution(passNoInputsR(allCards.cardRest), { paths: [] });
  x = addToSolution(passNoInputsE(allCards.cardCrew_0009), x.solution);
  x = addToSolution(passNoInputsE(allCards.cardBattle_0012), x.solution);
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

// console.log(level3Test());