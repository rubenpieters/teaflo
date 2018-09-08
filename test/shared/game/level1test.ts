import { Solution, Path, runSolution, SolCard, SolEvent, SolRest } from "src/shared/game/solution";
import { allCards, Card, Event, Rest } from "src/shared/game/card";
import { showSolutionLog } from "src/shared/game/log";
import { createCard } from "src/shared/game/ability";
import { IdCrew, GameState, CreatureId } from "src/shared/game/state";
import { onAllAlly } from "src/shared/game/crew";
import { TargetType } from "src/shared/game/target";

function test1() {
  const guardAction: Card = 
  {
      
    effects: [
      { effect: (_inputs: any[]) => { return (state: GameState, id: CreatureId) => {
        return onAllAlly(
          state,
          (_: IdCrew, id: number) => { return {
              tag: "QueueStatus",
              target: { 
                tag: "Target",
                type: "ally",
                position: id,
              },
              status: {
                tag: "Guard",
                value: 1,
                guard: 5,
                fragment: 0,
              },
            }
          }
        )
        }},
        inputs: [],
        description: "",
      },
    ],
    origin: { tag: "EntityOrigin", entityId: 2, entityType: "ally" },
    name: "",
    tag: "general",
  };

  const path1: Path = {
    restCard: passNoInputsR(allCards.cardRest),
    eventCards: [
      passNoInputsE(allCards.cardCrew_0003),
      passNoInputsE(allCards.cardCrew_0003),
      passNoInputsE(allCards.cardCrew_0004),
      passNoInputsE(allCards.cardCrew_0004),
      passNoInputsE(allCards.cardBattle_0009),
      passNoInputsE(guardAction),
      passNoInputsE(guardAction),
      passNoInputsE(guardAction),
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

function passNoInputsE(card: Event): SolEvent {
  return {
    ...card,
    effects: card.effects.map(x => { return {...x, effect: x.effect([])}; }),
  }
}

function passNoInputsR(card: Rest): SolRest {
  return {
    ...card,
    effects: card.effects.map(x => { return {...x, effect: x.effect([])}; }),
  }
}

test1();