import { ActionSpec, Action } from "src/shared/game/action";
import { TargetType } from "src/shared/game/target";
import { GameState } from "src/shared/game/state";

export type Trigger = {
  onTag: string,
  type: "before" | "instead",
  action: (a: Action) => (state: GameState, selfId: number, selfType: TargetType) => { action: Action, charges: number },
  charges: number,
};

export type OwnId = {
  tag: "OwnId"
};

export type TypeCondition = {
  tag: "TypeCondition",
  type: TargetType,
};

export type InPosition = {
  tag: "InPosition",
  position: number,
};


export type Condition
  = OwnId
  | TypeCondition
  | InPosition
  ;

export function showTrigger(
  trigger: Trigger,
) {
  return trigger.type + " " + trigger.onTag;
  // " (if " + trigger.conditions.map(showCondition).join(" & ") + " ):\n " + showAction(trigger.action);
}

export function showCondition(
  condition: Condition,
) {
  switch (condition.tag) {
    case "OwnId": {
      return "own id";
    }
    case "TypeCondition": {
      return `target is ${condition.type}`;
    }
    case "InPosition": {
      return `in position ${condition.position}`;
    }
  }
}

function hasTarget<T>(a: {}): a is { target: T } {
  return (<Object>a).hasOwnProperty("target");
}

function hasId(a: {}): a is { id: number } {
  return (<Object>a).hasOwnProperty("id");
}

function hasType(a: {}): a is { type: TargetType } {
  return (<Object>a).hasOwnProperty("type");
}

export function findIndex<A>(
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