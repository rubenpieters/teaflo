import { focus, over, set } from "src/shared/iassign-util";
import { Solution, runSolution, SolCard, SolEvent, SolRest, SolutionResult } from "src/shared/game/solution";
import { Event, Rest } from "src/shared/game/card";
import { GameState } from "src/shared/game/state";

export function addToSolution(
  card: SolCard, 
  solution: Solution,
  inputs: any[],
): { solution: Solution, result: SolutionResult } {
  switch (card.tag) {
    case "crew":
    case "enemy":
    case "item":
    case "general": {
      if (solution.paths.length === 0) {
        throw "no paths created yet";
      } else {
        solution = focus(solution, over(x => x.paths[solution.paths.length - 1].eventCards, x => x.concat({ event: card, inputs})));
      }
      break;
    }
    case "rest": {
      solution = focus(solution, over(x => x.paths, x => x.concat({ restCard: card, eventCards: [] })));
      break;
    }
  }

  return { solution, result: runSolution(solution) };
}

export function fSt(st: GameState | "invalid"): GameState {
  if (st === "invalid") {
    console.log("WARNING: forced invalid state");
    throw "invalid";
  } else {
    return st;
  }
}

/*
export function passNoInputsE(card: Event): SolEvent {
  return {
    ...card,
    effects: card.effects.map(x => { return {...x, effect: x.effect([])}; }),
  }
}

export function passNoInputsR(card: Rest): SolRest {
  return {
    ...card,
    effects: card.effects.map(x => { return {...x, effect: x.effect([])}; }),
  }
}
*/