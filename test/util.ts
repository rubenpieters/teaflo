import { GameState, possibleActions } from "../src/shared/game/state";
import { extendSolution, Solution, runPhases, SolutionData, showSolDataCompact } from "../src/shared/game/solution";
import { Location, getLocation } from "../src/shared/tree";

export function trySolutions(
  state: GameState,
  depth: number,
) {
  return _trySolutions(state, depth, []);
}

export function _trySolutions(
  state: GameState,
  depth: number,
  acc: SolutionData[],
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
    if (next.state.state === "invalid") {
      //console.log("invalid state");
    } else if (! next.win) {
      //console.log(`Trying branch ${branchIndex}`);
      _trySolutions(next.state, depth - 1, newAcc);
    } else if (next.win) {
      const show = newAcc
        .map((x, i) => `${i}:= ${showSolDataCompact(x)}`)
        .join("| => |")
        ;
      console.log(`Win reached (${newAcc.length}): |${show}|`);
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