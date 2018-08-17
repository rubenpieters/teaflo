import expect from "expect";
import { Solution, Path, runSolution } from "src/shared/game/solution";
import { allCards } from "src/shared/game/card";
import { showSolutionLog } from "src/shared/game/log";
import { abilityIdToAction, createCard } from "src/shared/game/ability";

function test1() {
  const path1: Path = {
    restCard: allCards.cardRest,
    eventCards: [
      allCards.cardCrew_0003,
      allCards.cardCrew_0003,
      allCards.cardCrew_0004,
      allCards.cardCrew_0004,
      allCards.cardBattle_0009,
      //createCard(abilityIdToAction(0, board.lastState!, 2)),
      {
        actions: [
          {
            tag: "QueueStatus",
            target: { 
              tag: "Target",
              type: "ally",
              positions: [0, 1, 2, 3],
            },
            status: {
              tag: "Guard",
              value: 1,
              guard: 5,
            },
          }
        ],
        id: 0,
        name: "",
        tag: "event",
        subtag: "general"
      },
      {
        actions: [
          {
            tag: "QueueStatus",
            target: { 
              tag: "Target",
              type: "ally",
              positions: [0, 1, 2, 3],
            },
            status: {
              tag: "Guard",
              value: 1,
              guard: 5,
            },
          }
        ],
        id: 0,
        name: "",
        tag: "event",
        subtag: "general"
      },
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
    //console.log(JSON.stringify(state, undefined, 2));
    console.log(showSolutionLog(log));
  }
}

test1();