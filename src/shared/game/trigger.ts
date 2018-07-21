import { ActionSpec, ActionTarget } from "src/shared/game/action";
import { TargetSpec, TargetType } from "src/shared/game/target";
import { GameState } from "src/shared/game/state";

export type Trigger = {
  onTag: string,
  type: "before", // | "after" | "on"
  action: ActionSpec,
  conditions: Condition[],
};

export type OwnId = {
  tag: "OwnId"
};

export type ApCondition = {
  tag: "ApCondition"
};

export type Condition
  = OwnId
  | ApCondition
  ;

export function checkConditions(
  conditions: Condition[],
  action: ActionTarget,
  state: GameState,
  selfId: number,
  selfType: TargetType,
) {
  return conditions.reduce((acc, cond) => {
    if (acc) {
      return checkCondition(cond, action, state, selfId, selfType);
    } else {
      return false;
    }
  }, true);
}

export function checkCondition(
  condition: Condition,
  action: ActionTarget,
  state: GameState,
  selfId: number,
  selfType: TargetType,
) {
  switch (condition.tag) {
    case "OwnId": {
      if (hasTarget(action)) {
        return findIndex(x => x === selfId, action.target.positions) !== "notFound";
      } else if (hasId(action)) {
        return action.id === selfId;
      } else {
        throw "action " + action.tag + " does not have a target!";
      }
    }
    case "ApCondition": {
      return true;
    }
  }
}

function hasTarget<T>(a: {}): a is { target: T } {
  return (<Object>a).hasOwnProperty("target");
}

function hasId(a: {}): a is { id: number } {
  return (<Object>a).hasOwnProperty("id");
}

function findIndex<A>(
  predicate: (a: A) => boolean,
  as: A[],
): number | "notFound" {
  let index: number = 0;
  for (const a of as) {
    if (predicate(a)) {
      return index;
    }
    index += 1;
  }
  return "notFound";
}