import { GameState } from "./state";
import { Context } from "./context";
import { ActionF, Condition_URI, Action } from "./action";
import { UnitId } from "./entityId";
import deepEqual from "deep-equal";
import { StStatus } from "./statusRow";

export class StatusCondition {
  public readonly tag: "StatusCondition" = "StatusCondition";

  constructor(
    public readonly actionCondition: ActionCondition,
  ) {}
}

export type Condition
  = StatusCondition
  ;

export function resolveCondition(
  status: StStatus,
  actionCondition: ActionCondition,
  onStackAction: Action,
): { condition: boolean, bindings: { [K in string]: any } } {
  if (onStackAction.tag === actionCondition.tag) {
    const keys = Object.keys(actionCondition);
    let condition = true;
    let bindings: { [K in string]: any } = {};
    for (const key of keys) {
      if (key !== "tag" && key !== "uriF" && key !== "uriG") {
        const conditionVar = (actionCondition as any)[key];
        const val = (onStackAction as any)[key];
        const resolved = resolveConditionVar(status, conditionVar, val);
        if (resolved.binding !== undefined) {
          bindings[resolved.binding[0]] = resolved.binding[1];
        }
        if (! resolved.result) {
          condition = false;
        }
      }
    }
    return { condition, bindings };
  } else {
    return { condition: false, bindings: {} };
  }
}

export function resolveConditionVar<A>(
  status: StStatus,
  conditionVar: ConditionVar<A>,
  val: A,
): { result: boolean, binding?: [string, A] } {
  switch (conditionVar.tag) {
    case "Static": {
      const result = deepEqual(val, conditionVar.a);
      return { result };
    }
    case "StatusOwner": {
      const result = deepEqual(val, status.owner);
      return { result };
    }
    case "Var": {
      return { result: true, binding: [conditionVar.bindingName, val] };
    }
    case "StatusValue": {
      const result = deepEqual(val, status.value);
      return { result };
    }
  }
}

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Cond: ConditionVar<A>,
  }
}

export type ActionCondition = ActionF<Condition_URI, Condition_URI>;

/**
 * A Condition Variable is a description of how a parameter of an action should be compared in a condition.
 */
export type ConditionVar<A>
  = Static<A>
  | Var
  | StatusOwner
  | StatusValue
  ;

export class Static<A> {
  public readonly tag: "Static" = "Static";

  constructor (
    public readonly a: A,
  ) {}
}

export class Var {
  public readonly tag: "Var" = "Var";

  constructor(
    public readonly bindingName: string,
  ) {}
}

export class StatusOwner {
  public readonly tag: "StatusOwner" = "StatusOwner";
  
  constructor(

  ) {}
}

export function statusOwner(): ConditionVar<UnitId> {
  return new StatusOwner();
}

export class StatusValue {
  public readonly tag: "StatusValue" = "StatusValue";
  
  constructor(

  ) {}
}

export function statusValue(): ConditionVar<number> {
  return new StatusValue();
}