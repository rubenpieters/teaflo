import { focus, over, set } from "src/shared/iassign-util";
import { Card, Rest, Event } from "src/shared/game/card";
import { Action, ActionTarget, applyActionAndTriggers, enemyTurn, checkDeaths, checkStatusCrew, checkStatusEnemy } from "src/shared/game/action";
import { GameState, initialState } from "src/shared/game/state";
import { SolutionLog, ActionLog, emptySolutionLog } from "src/shared/game/log";
import { Target } from "src/shared/game/target";
import { Generator, plusOneGenerator } from "src/shared/handler/id/generator";

export type Path = {
  restCard: Rest,
  eventCards: Event[],
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
): ActionTarget {
  const path: Path | undefined = solution.paths[index.path];
  if (path === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  if (index.card === "rest") {
    return path.restCard.actions[index.action];
  }
  const card: Card | undefined = path.eventCards[index.card];
  if (card === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  const action: Action<Target> | undefined = card.actions[index.action];
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

  newAction += 1;

  if (newCard === "rest") {
    if (newAction < solution.paths[newPath].restCard.actions.length) {
      return focus(index, set(x => x.path, newPath), set(x => x.card, newCard), set(x => x.action, newAction));
    } else {
      newCard = 0;
      newAction = 0;
    }
  }

  if (
    newCard < solution.paths[newPath].eventCards.length &&
    newAction < solution.paths[newPath].eventCards[newCard].actions.length
  ) {
    return focus(index, set(x => x.path, newPath), set(x => x.card, newCard), set(x => x.action, newAction));
  }

  newCard += 1;
  newAction = 0;

  if (
    newCard < solution.paths[newPath].eventCards.length &&
    newAction < solution.paths[newPath].eventCards[newCard].actions.length
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
  let log: ActionTarget[] = [];
  if (index.action === 0) {
    const afterEnemyStatus = checkStatusEnemy(state, log, idGen);
    log = afterEnemyStatus.log;
    if (afterEnemyStatus.state === "invalid") {
      return { result: "invalid", log: { action, loggedEffects: log } };
    } else {
      state = afterEnemyStatus.state;
    }

    const afterCrewStatus = checkStatusCrew(state, log, idGen);
    log = afterCrewStatus.log;
    if (afterCrewStatus.state === "invalid") {
      return { result: "invalid", log: { action, loggedEffects: log } };
    } else {
      state = afterCrewStatus.state;
    }

    const afterEnemy = enemyTurn(state, log, idGen);
    log = afterEnemy.log;
    if (afterEnemy.state === "invalid") {
      return { result: "invalid", log: { action, loggedEffects: log } };
    } else {
      state = afterEnemy.state;
    }
  }
  const actionResult = applyActionAndTriggers(action, state, log, idGen, "noOrigin");
  const actionLog: ActionLog = {
    action: action,
    loggedEffects: actionResult.log,
  };

  if (actionResult.state === "invalid") {
    return { result: "invalid", log: actionLog };
  }
  state = actionResult.state;
  log = actionResult.log;

  const afterDeaths = checkDeaths(state, log, idGen);
  if (afterDeaths.state === "invalid") {
    return { result: "invalid", log: actionLog };
  }
  state = afterDeaths.state;
  log = afterDeaths.log;

  const newIndex = nextIndex(index, solution);

  return { result: { newIndex, newState: state }, log: { action: action, loggedEffects: log } };
}

export type SolutionResult = { state: GameState | "invalid", log: SolutionLog };

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
  const newSolutionLog = focus(log, over(x => x.actionLog, x => x.concat(solStep.log)));
  if (stepResult === "invalid") {
    return { state: "invalid", log: newSolutionLog };
  }
  const { newIndex, newState } = stepResult;
  return _runSolution(solution, newIndex, newState, newSolutionLog, idGen);
}

export function runSolutionAll(
  solution: Solution,
): SolutionResult[] {
  if (solution.paths.length === 0) {
    return [];
  }
  return _runSolutionAll(solution, initialIndex, initialState, emptySolutionLog, plusOneGenerator(), []);
}

function _runSolutionAll(
  solution: Solution,
  index: SolutionIndex | "done",
  state: GameState,
  log: SolutionLog,
  idGen: Generator,
  acc: SolutionResult[],
): SolutionResult[] {
  if (index === "done") {
    return acc;
  }

  const solStep = solutionStep(index, state, solution, idGen);
  const stepResult = solStep.result;
  const newSolutionLog = focus(log, over(x => x.actionLog, x => x.concat(solStep.log)));
  if (stepResult === "invalid") {
    return acc.concat({ state: "invalid", log: newSolutionLog });
  } else {
    const { newIndex, newState } = stepResult;
    acc = acc.concat({ state: newState, log: newSolutionLog });
    return _runSolutionAll(solution, newIndex, newState, newSolutionLog, idGen, acc);
  }
}