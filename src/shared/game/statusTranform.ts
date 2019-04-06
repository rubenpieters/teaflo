import { ConditionVar, resolveConditionVar } from "./condition";
import { Ability_URI, ActionF, Target_URI, Damage, Action, UseCharge, Death, Combined, CombinedAction, StatusTransform_URI, hoistActionF } from "./action";
import { GameState } from "./state";
import { Context } from "./context";

export class Monus {
  public readonly tag: "Monus" = "Monus";

  constructor(
    public readonly v1: StatusTransformVar<number>,
    public readonly v2: StatusTransformVar<number>,
  ) {}
}

export function monus(
  v1: StatusTransformVar<number>,
  v2: StatusTransformVar<number>,
): StatusTransformVar<number> {
  return new Monus(v1, v2);
}

export class Add {
  public readonly tag: "Add" = "Add";

  constructor(
    public readonly v1: StatusTransformVar<number>,
    public readonly v2: StatusTransformVar<number>,
  ) {}
}

export function add(
  v1: StatusTransformVar<number>,
  v2: StatusTransformVar<number>,
): StatusTransformVar<number> {
  return new Add(v1, v2);
}

export type StatusTransformVar<A>
  = ConditionVar<A>
  | Monus
  | Add
  ;

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    ST: StatusTransformVar<A>,
  }
}

export type StatusTransform = ActionF<StatusTransform_URI, StatusTransform_URI>;

export function resolveStatusTransform(
  state: GameState,
  statusTransform: StatusTransform,
  context: Context,
  bindings: { [K in string]: any },
): Action {
  const f = <A>(v: StatusTransformVar<A>) => resolveStatusTransformVar(state, v, context, bindings);
  const action = hoistActionF(statusTransform, "Action", "Action", f, f);
  return action;
}

export function resolveStatusTransformVar<A>(
  state: GameState,
  statusTransformVar: StatusTransformVar<A>,
  context: Context,
  bindings: { [K in string]: any },
): A {
  switch (statusTransformVar.tag) {
    case "Add": {
      const v1 = resolveStatusTransformVar(state, statusTransformVar.v1, context, bindings);
      const v2 = resolveStatusTransformVar(state, statusTransformVar.v2, context, bindings);
      const val = v1 + v2;
      return val as any;
    }
    case "Monus": {
      const v1 = resolveStatusTransformVar(state, statusTransformVar.v1, context, bindings);
      const v2 = resolveStatusTransformVar(state, statusTransformVar.v2, context, bindings);
      const val = v1 - v2;
      return Math.max(0, val) as any;
    }
    case "Static": {
      return statusTransformVar.a;
    }
    case "StatusOwner": {
      if (context.tag !== "StatusContext") {
        throw `resolveConditionVar: Invalid Context ${context.tag}, expected StatusContext`;
      }
      return context.owner as any;
    }
    case "StatusValue": {
      throw "unimpl";
    }
    case "Var": {
      return bindings[statusTransformVar.bindingName];
    }
  }
}