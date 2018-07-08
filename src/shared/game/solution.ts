import { focus, over, set } from "src/shared/iassign-util";
import { Action, ActionRest, Recruit, Battle, Rest, doAction } from "src/shared/game/action";
import { GameState, initialState } from "src/shared/game/state";
import { SolutionLog, ActionLog, emptySolutionLog } from "src/shared/game/log";
import { Target } from "src/shared/game/target";
import { Generator, plusOneGenerator } from "src/shared/handler/id/generator";

export type Card = Action<Target>[];

export type Path = {
  restAction: Rest,
  cards: Card[]
};

export type Solution = {
  paths: Path[]
};

export type SolutionIndex = {
  path: number,
  card: "rest" | number,
  action: number,
};

export const initialIndex: SolutionIndex = {
  path: 0,
  card: "rest",
  action: 0,
};

function nextAction(
  index: SolutionIndex,
  solution: Solution
): ActionRest {
  const path: Path | undefined = solution.paths[index.path];
  if (path === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  if (index.card === "rest") {
    return path.restAction;
  }
  const card: Card | undefined = path.cards[index.card];
  if (card === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  const action: Action<Target> | undefined = card[index.action];
  if (action === undefined) {
    throw ("invalid index " + JSON.stringify(index));
  }
  return action;
}

export function nextIndex(
  index: SolutionIndex,
  solution: Solution,
): SolutionIndex | "done" {
  let newPath: number = index.path;
  let newCard: "rest" | number = index.card;
  let newAction: number = index.action;

  if (newCard === "rest") {
    newCard = 0;
    newAction = 0;
  } else {
    newAction += 1;
  }

  if (
    newCard < solution.paths[newPath].cards.length &&
    newAction < solution.paths[newPath].cards[newCard].length
  ) {
    return focus(index, set(x => x.path, newPath), set(x => x.card, newCard), set(x => x.action, newAction));
  }

  newCard += 1;
  newAction = 0;

  if (
    newCard < solution.paths[newPath].cards.length &&
    newAction < solution.paths[newPath].cards[newCard].length
  ) {
    return focus(index, set(x => x.path, newPath), set(x => x.card, newCard), set(x => x.action, newAction));
  }

  newPath += 1;
  newCard = "rest";
  newAction = 0;

  if (newPath < solution.paths.length) {
    return focus(index, set(x => x.path, newPath), set(x => x.card, newCard), set(x => x.action, newAction));
  } else {
    return "done";
  }
}

function solutionStep(
  index: SolutionIndex,
  state: GameState,
  solution: Solution,
  idGen: Generator,
): { result: "invalid" | { newIndex: "done" | SolutionIndex, newState: GameState }, log: ActionLog } {
  const action = nextAction(index, solution);
  const actionResult = doAction(action, state, [], 0, idGen);
  const actionLog: ActionLog = {
    action: action,
    loggedEffects: actionResult.newLog,
  };

  if (actionResult.newState === "invalid") {
    return { result: "invalid", log: actionLog };
  }
  const newIndex = nextIndex(index, solution);

  return { result: { newIndex, newState: actionResult.newState }, log: actionLog };
}

export type SolutionResult = { state: GameState, log: SolutionLog } | "invalid";

export function runSolution(
  solution: Solution,
): SolutionResult {
  if (solution.paths.length === 0) {
    return { state: initialState, log: { actionLog: [] } };
  }
  return _runSolution(solution, initialIndex, initialState, emptySolutionLog, plusOneGenerator());
}

function _runSolution(
  solution: Solution,
  index: SolutionIndex | "done",
  state: GameState,
  log: SolutionLog,
  idGen: Generator,
): SolutionResult {
  if (index === "done") {
    return { state, log };
  }

  const solStep = solutionStep(index, state, solution, idGen);
  const stepResult = solStep.result;
  if (stepResult === "invalid") {
    return "invalid";
  }
  const { newIndex, newState } = stepResult;
  const newSolutionLog = focus(log, over(x => x.actionLog, x => x.concat([solStep.log])));
  return _runSolution(solution, newIndex, newState, newSolutionLog, idGen);
}