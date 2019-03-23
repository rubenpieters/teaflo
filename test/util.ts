import { GameState, possibleActions, showStateCompact } from "../src/shared/game/state";
import { extendSolution, Solution, runPhases, SolutionData, showSolDataCompact } from "../src/shared/game/solution";
import { Location, getLocation } from "../src/shared/tree";
import { fnv1a } from "../src/shared/fnv1a";
import deepequal from "deep-equal";

export function trySolutions(
  state: GameState,
  depth: number,
) {
  return _trySolutions(state, depth, [], {});
}

type StateCache = {
  [hash: number]: string[],
}

function isInCache(
  state: GameState,
  cache: StateCache,
): boolean {
  const stateString = showStateCompact(state);
  const hash = fnv1a(stateString);
  if (cache[hash] === undefined) {
    cache[hash] = [stateString];
    return false;
  } else {
    const inCache = cache[hash].some(el => {
      return el === stateString;
    });
    if (inCache) {
      return true;
    } else {
      cache[hash].push(stateString);
      return false;
    }
  }
}

export function _trySolutions(
  state: GameState,
  depth: number,
  acc: SolutionData[],
  cache: StateCache,
) {
  if (depth <= 0) {
    //console.log("reached max depth");
    return;
  }
  const nextStates = tryNextActions(state);
  const wins = nextStates.filter(x => x.win).length;
  //console.log(`${wins} win(s)`);
  nextStates.forEach((next, branchIndex) => {
    const newAcc = acc.concat(next.action);
    if (isInCache(next.state, cache)) {
      //console.log("Skipping, already seen in cache");
    } else if (next.state.state === "invalid") {
      //console.log("invalid state");
    } else if (! next.win) {
      //console.log(`Trying branch ${branchIndex}`);
      _trySolutions(next.state, depth - 1, newAcc, cache);
    } else if (next.win) {
      const show = newAcc
        .map((x, i) => `${showSolDataCompact(x)}`)
        .join("\n")
        ;
      console.log(showStateCompact(next.state));
      console.log(`Win reached (${newAcc.length}):\n${show}\n----`);
    }
  });
}

export function tryNextActions(
  state: GameState,
): {
  state: GameState,
  win: boolean,
  action: SolutionData,
}[] {
  const actions = possibleActions(state);
  //console.log(`Trying ${actions.length} actions`);
  return actions.map(action => {
    const result = runPhases(state, action);
    return { ...result, action, };
  });
}