import expect from "expect";
import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCards } from "src/shared/game/card";
import { showSolutionLog } from "src/shared/game/log";

function test1() {
  const path1: Path = {
    restCard: allCards.cardRest,
    eventCards: [
      allCards.cardCrew_0003,
      allCards.cardCrew_0003,
      allCards.cardCrew_0004,
      allCards.cardCrew_0004,
      allCards.cardBattle_0009,
    ]
  }
  const solution: Solution = {
    paths: [path1]
  }

  const sol = runSolution(solution);
  if (sol.state === "invalid") {
    console.log(showSolutionLog(sol.log));
    console.log("invalid");
  } else {
    const { state, log } = sol;
    console.log(JSON.stringify(state, undefined, 2));
    console.log(showSolutionLog(log));
  }
}

test1();