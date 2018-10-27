import { focus, over, set } from "src/shared/iassign-util";
import { Tree, extendTree, Location } from "src/shared/tree";
import { Card, CardOrigin } from "src/shared/game/card";
import { GameState, initialState } from "src/shared/game/state";
import { Action, applyActionAndTriggers, applyActionQueue, enemyTurn, checkDeaths } from "src/shared/game/action";
import { Origin } from "src/shared/game/target";
import { ActionLog, ApplyActionLog, SolutionLog } from "src/shared/game/log";
import { Generator, plusOneGenerator } from "src/shared/handler/id/generator";
import { applyLoseFragmentPhase } from "./status";

type CardInput = Card & { inputs: any[] };

export type Solution = Tree<CardInput>;

export function extendSolution(
  card: CardInput,
  solution: Solution,
  loc: Location,
): Solution {
  return extendTree(equalId, solution, loc, card);
}

function equalId<C extends { origin: CardOrigin }>(
  card1: C,
  card2: C,
) {
  return false;
  /*if (card1.origin.tag === "PlayerOrigin" || card2.origin.tag === "PlayerOrigin") {
    return false;
  }
  return card1.origin.id === card2.origin.id;*/
}

function runAction(
  state: GameState,
  cardEffect: Action,
  cardOrigin: Origin,
  idGen: Generator,
): { state: GameState | "invalid", log: ActionLog } {
  // TODO: review phases

  let log: ActionLog = {
    action: cardEffect,
    startTurn: [], loseFragment: [], crewAction: [], queue1: [],
    allyInstanceAction: [],
    enemyAction: [], queue2: [],
    enemyInstanceAction: [],
    deaths: [],
  };

  // Apply Turn Action
  let actionResult: { state: GameState | "invalid", log: ApplyActionLog } =
    applyActionAndTriggers(cardEffect, state, [], idGen, cardOrigin);
  
  log = focus(log, set(x => x.crewAction, actionResult.log));
  if (actionResult.state === "invalid") {
    return { state: "invalid", log };
  }
  state = actionResult.state;

  // TODO: deaths and addcrew/enemy in action queue
  const afterQueue1 = applyActionQueue(state, [], idGen);
  log = focus(log, set(x => x.queue1, afterQueue1.log));
  if (afterQueue1.state === "invalid") {
    return { state: "invalid", log };
  }
  state = afterQueue1.state;

  // Apply StartTurn effect
  let aferStartTurn: { state: GameState | "invalid", log: ApplyActionLog } =
    applyActionAndTriggers({ tag: "StartTurn" }, state, [], idGen, "noOrigin");
  log = focus(log, set(x => x.startTurn, aferStartTurn.log));
  if (aferStartTurn.state === "invalid") {
    return { state: "invalid", log };
  }
  state = aferStartTurn.state;

  // Lose Fragment Phase
  const afterLoseFragment = applyLoseFragmentPhase(state, [], idGen, "noOrigin");
  log = focus(log, set(x => x.loseFragment, afterLoseFragment.log));
  if (afterLoseFragment.state === "invalid") {
    return { state: "invalid", log };
  }
  state = afterLoseFragment.state;

  // Ally Instance Action Phase
  for (const instance of state.allyInstances) {
    const result = applyActionAndTriggers(
      instance.action.effect({ state }).action, state, [], idGen, "noOrigin");
      log = focus(log, over(x => x.allyInstanceAction, x => x.concat(result.log)));
      if (result.state === "invalid") {
        return { state: "invalid", log };
      }
      state = result.state;
  }

  // Enemy Action Phase
  const afterEnemy = enemyTurn(state, [], idGen);
  log = focus(log, set(x => x.enemyAction, afterEnemy.log));
  if (afterEnemy.state === "invalid") {
    return { state: "invalid", log };
  }
  state = afterEnemy.state;

  // TODO: deaths and addcrew/enemy in action queue
  const afterQueue2 = applyActionQueue(state, [], idGen);
  log = focus(log, set(x => x.queue2, afterQueue2.log));
  if (afterQueue2.state === "invalid") {
    return { state: "invalid", log };
  }
  state = afterQueue2.state;

  // Enemy Instance Action Phase
  for (const instance of state.enemyInstances) {
    const result = applyActionAndTriggers(
      instance.action.effect({ state }).action, state, [], idGen, "noOrigin");
      log = focus(log, over(x => x.enemyInstanceAction, x => x.concat(result.log)));
      if (result.state === "invalid") {
        return { state: "invalid", log };
      }
      state = result.state;
  }

  const afterDeaths = checkDeaths(state, [], idGen);
  log = focus(log, set(x => x.deaths, afterDeaths.log));
  if (afterDeaths.state === "invalid") {
    return { state: "invalid", log };
  }
  state = afterDeaths.state;

  return { state, log };
}

function runCard(
  state: GameState,
  card: Card,
  idGen: Generator,
): { state: GameState | "invalid", log: ActionLog[] } {
  let log: ActionLog[] = [];
  for (const action of card.effects) {
    let cardEffect: Action = <any>undefined;
    let cardOrigin: Origin = <any>undefined;
    switch (card.origin.tag) {
      case "PositionId":
      case "GlobalId": {
        cardEffect = action.effect(
          { state, selfId: card.origin, inputs: action.inputs }
        ).action;
        cardOrigin = card.origin;
        break;
      }
      case "PlayerOrigin": {
        cardEffect = action.effect({ state }).action;
        cardOrigin = "noOrigin";
        break;
      }
    }

    const result = runAction(state, cardEffect, cardOrigin, idGen);
    log = log.concat(result.log);
    if (result.state === "invalid") {
      return { state, log };
    }
    state = result.state;
  }
  return { state, log };
}

export function runSolution(
  solution: Solution,
  loc: Location,
) {
  return _runSolution(initialState, loc, solution, { actionLog: [] }, plusOneGenerator());
}

// run till location : runs from the starting point to that location
function _runSolution(
  state: GameState,
  loc: Location,
  solution: Solution,
  log: SolutionLog,
  idGen: Generator,
): { state: GameState | "invalid", log: SolutionLog } {
  if (loc.length === 0) {
    return { state, log };
  }
  const i = loc[0];
  const result = runCard(state, solution.nodes[i].v, idGen);
  log = { actionLog: log.actionLog.concat(result.log) }
  if (result.state === "invalid") {
    return { state: "invalid", log };
  }
  state = result.state;
  if (loc.length === 1) {
    return { state, log };
  }
  return _runSolution(state, loc.slice(1,), solution.nodes[i].tree, log, idGen);
}
