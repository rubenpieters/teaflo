import { focus, over, set } from "src/shared/iassign-util";
import { Action, Recruit, Battle, Rest, doAction } from "src/shared/game/action";
import { GameState, initialState } from "src/shared/game/state";
import { SolutionLog, ActionLog, emptySolutionLog } from "src/shared/game/log";

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
): { result: "invalid" | { newIndex: "done" | SolutionIndex, newState: GameState }, log: ActionLog } {
  const action = nextAction(index, solution);
  const actionResult = doAction(action, state);
  const actionLog: ActionLog = {
    action: action,
    loggedEffects: actionResult.log,
  }

  if (actionResult.newState === "invalid") {
    return { result: "invalid", log: actionLog };
  }
  const newIndex = nextIndex(index, solution);

  return { result: { newIndex, newState: actionResult.newState }, log: actionLog };
}

export function runSolution(
  solution: Solution,
): { state: GameState, log: SolutionLog } | "invalid" {
  return _runSolution(solution, initialIndex, initialState, emptySolutionLog);
}

function _runSolution(
  solution: Solution,
  index: SolutionIndex | "done",
  state: GameState,
  log: SolutionLog,
): { state: GameState, log: SolutionLog } | "invalid" {
  if (index === "done") {
    return { state, log };
  }

  const solStep = solutionStep(index, state, solution);
  const stepResult = solStep.result;
  if (stepResult === "invalid") {
    return "invalid";
  }
  const { newIndex, newState } = stepResult;
  const newSolutionLog = focus(log, over(x => x.actionLog, x => x.concat([solStep.log])));
  return _runSolution(solution, newIndex, newState, newSolutionLog);
}