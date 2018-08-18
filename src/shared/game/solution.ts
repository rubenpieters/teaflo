import { focus, over, set } from "src/shared/iassign-util";
import { Card, Rest, Event } from "src/shared/game/card";
import { ActionTarget, applyActionAndTriggers, enemyTurn, checkDeaths, checkStatusCrew, checkStatusEnemy, applyActionQueue } from "src/shared/game/action";
import { GameState, initialState } from "src/shared/game/state";
import { SolutionLog, ActionLog, emptySolutionLog } from "src/shared/game/log";
import { Origin } from "src/shared/game/target";
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
): { action: ActionTarget, origin: Origin } {
  const path: Path | undefined = solution.paths[index.path];
  if (path === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  if (index.card === "rest") {
    return { action: path.restCard.actions[index.action], origin: "noOrigin" };
  }
  const card: Card | undefined = path.eventCards[index.card];
  if (card === undefined) {
    throw ("invalid index: " + JSON.stringify(index));
  }
  const action: ActionTarget | undefined = card.actions[index.action];
  if (action === undefined) {
    throw ("invalid index " + JSON.stringify(index));
  }
  return { action, origin: card.origin ? card.origin : "noOrigin" };
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
  const { action, origin } = nextAction(index, solution);
  let log: ActionLog = { action, crewStatus: [], crewAction: [], queue1: [], enemyStatus: [], enemyAction: [], queue2: [], deaths: [] };

  const afterCrewStatus = checkStatusCrew(state, idGen);
  log = focus(log, set(x => x.crewStatus, afterCrewStatus.log));
  if (afterCrewStatus.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterCrewStatus.state;

  // TODO: add crew auto-actions?

  const actionResult = applyActionAndTriggers(action, state, [], idGen, origin);
  log = focus(log, set(x => x.crewAction, actionResult.log));
  if (actionResult.state === "invalid") {
    return { result: "invalid", log };
  }
  state = actionResult.state;

  // TODO: deaths and addcrew/enemy in action queue
  const afterQueue1 = applyActionQueue(state, [], idGen);
  log = focus(log, set(x => x.queue1, afterQueue1.log));
  if (afterQueue1.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterQueue1.state;

  const afterEnemyStatus = checkStatusEnemy(state, idGen);
  log = focus(log, set(x => x.enemyStatus, afterEnemyStatus.log));
  if (afterEnemyStatus.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterEnemyStatus.state;

  const afterEnemy = enemyTurn(state, [], idGen);
  log = focus(log, set(x => x.enemyAction, afterEnemy.log));
  if (afterEnemy.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterEnemy.state;

  // TODO: deaths and addcrew/enemy in action queue
  const afterQueue2 = applyActionQueue(state, [], idGen);
  log = focus(log, set(x => x.queue2, afterQueue2.log));
  if (afterQueue2.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterQueue2.state;

  const afterDeaths = checkDeaths(state, [], idGen);
  log = focus(log, set(x => x.deaths, afterDeaths.log));
  if (afterDeaths.state === "invalid") {
    return { result: "invalid", log };
  }
  state = afterDeaths.state;

  const newIndex = nextIndex(index, solution);
  return { result: { newIndex, newState: state }, log };
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