import { GameState } from "./state";
import { Context } from "./context";
import { ActionF, Condition_URI, Action } from "./action";
import { UnitId } from "./entityId";
import deepEqual from "deep-equal";

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
  state: GameState,
  actionCondition: ActionCondition,
  onStackAction: Action,
  context: Context,
): { condition: boolean, bindings: { [K in number]: any } } {
  if (onStackAction.tag === actionCondition.tag) {
    const keys = Object.keys(actionCondition);
    let condition = true;
    let bindings: { [K in string]: any } = {};
    for (const key of keys) {
      if (key !== "tag" && key !== "uriF" && key !== "uriG") {
        const conditionVar = (actionCondition as any)[key];
        const val = (onStackAction as any)[key];
        const resolved = resolveConditionVar(state, conditionVar, val, context);
        if (resolved.binding !== undefined) {
          bindings[key] = resolved.binding;
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
  state: GameState,
  conditionVar: ConditionVar<A>,
  val: A,
  context: Context,
): { result: boolean, binding?: A } {
  switch (conditionVar.tag) {
    case "Static": {
      const result = deepEqual(val, conditionVar.a);
      return { result };
    }
    case "StatusOwner": {
      if (context.tag !== "StatusContext") {
        throw `resolveConditionVar: Invalid Context ${context.tag}, expected StatusContext`;
      }
      const result = deepEqual(val, context.owner);
      return { result };
    }
    case "Var": {
      return { result: true, binding: val };
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