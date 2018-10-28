import { focus, over, set } from "src/shared/iassign-util";
import { Solution, runSolution, extendSolution } from "src/shared/game/solution";
import { GameState } from "src/shared/game/state";
import { solCardFromAbility } from "src/shared/game/ability";
import { Card } from "src/shared/game/card";
import { SolutionLog } from "src/shared/game/log";
import { Location } from "src/shared/tree";

export function addToSolution(
  card: Card, 
  prev: { solution: Solution, loc: Location},
  inputs: any[],
): { solution: Solution, loc: Location, result: { state: GameState | "invalid", log: SolutionLog } } {
  const solution = extendSolution({...card, inputs}, prev.solution, prev.loc);
  const newLoc = prev.loc.concat(0);

  return { solution, loc: newLoc, result: runSolution(solution, newLoc) };
}

export function useAbility(
  state: GameState | "invalid",
  allyId: number,
  abilityId: number,
) {
  return solCardFromAbility(
    fSt(state).crew[allyId].abilities[abilityId], { tag: "PositionId", id: allyId, type: "ally" });
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