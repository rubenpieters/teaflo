import { focus, over, set } from "src/shared/iassign-util";
import { Action, Recruit, Battle, Rest, doAction } from "src/shared/game/action";
import { GameState, initialState } from "src/shared/game/state";

export type Path = {
  restAction: Rest,
  actions: Action[]
}
export type Solution = {
  paths: Path[]
}


type SolutionIndex = {
  path: number,
  action: "rest" | number,
}

const initialIndex: SolutionIndex = {
  path: 0,
  action: "rest",
}

function nextAction(
  index: SolutionIndex,
  solution: Solution
): Action | Rest {
  const path: Path | undefined = solution.paths[index.path];
  if (path === undefined) {
    throw ("invalid index: " + index)
  } else {
    if (index.action === "rest") {
      return path.restAction;
    } else {
      const action: Action | undefined = path.actions[index.action];
      if (action === undefined) {
        throw ("invalid index: " + index)
      } else {
        return action;
      }
    }
  }
}

function nextIndex(
  index: SolutionIndex,
  solution: Solution,
): SolutionIndex | "done" {
  let newAction: "rest" | number;
  if (index.action === "rest") {
    newAction = 0;
  } else {
    newAction = index.action + 1;
  }
  const path: Path | undefined = solution.paths[index.path];
  if (path === undefined) {
    throw ("invalid index: " + index)
  } else {
    if (newAction < path.actions.length) {
      return focus(index, set(x => x.action, newAction));
    } else {
      if (index.path === solution.paths.length - 1) {
        return "done";
      } else {
        return focus(index,
          set(x => x.action, "rest"),
          over(x => x.path, x => x + 1),
        );
      }
    }
  }
}

function solutionStep(
  index: SolutionIndex,
  state: GameState,
  solution: Solution,
) {
  const action = nextAction(index, solution);

  const newState = doAction(action, state);
  if (newState === "invalid") {
    return "invalid";
  }
  const newIndex = nextIndex(index, solution);

  return { newIndex, newState };
}

export function runSolution(
  solution: Solution,
): GameState | "invalid" {
  return _runSolution(solution, initialIndex, initialState);
}

function _runSolution(
  solution: Solution,
  index: SolutionIndex | "done",
  state: GameState,
): GameState | "invalid" {
  if (index === "done") {
    return state;
  }

  const stepResult = solutionStep(index, state, solution);
  if (stepResult === "invalid") {
    return "invalid";
  }
  const { newIndex, newState } = stepResult;
  return _runSolution(solution, newIndex, newState);
}