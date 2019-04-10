import { ConditionVar, resolveConditionVar } from "./condition";
import { Ability_URI, ActionF, Target_URI, Damage, Action, UseCharge, Death, Combined, combinedAction, StatusTransform_URI, hoistActionF, ActionWithOrigin, hoistActionWithOriginF, ActionWithOriginF } from "./action";
import { GameState } from "./state";
import { Context } from "./context";
import { StStatus } from "./statusRow";

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

export type StatusTransform = ActionWithOriginF<StatusTransform_URI>;

export function resolveStatusTransform(
  statusTransform: StatusTransform,
  bindings: { [K in string]: any },
  status: StStatus,
  onStackAction: ActionWithOrigin,
): ActionWithOrigin {
  const f = <A>(v: StatusTransformVar<A>) => resolveStatusTransformVar(v, bindings, status, onStackAction);
  const action = hoistActionWithOriginF(statusTransform, "Action", f);
  return action;
}

export function resolveStatusTransformVar<A>(
  statusTransformVar: StatusTransformVar<A>,
  bindings: { [K in string]: any },
  status: StStatus,
  onStackAction: ActionWithOrigin,
): A {
  switch (statusTransformVar.tag) {
    case "Add": {
      const v1 = resolveStatusTransformVar(statusTransformVar.v1, bindings, status, onStackAction);
      const v2 = resolveStatusTransformVar(statusTransformVar.v2, bindings, status, onStackAction);
      const val = v1 + v2;
      return val as any;
    }
    case "Monus": {
      const v1 = resolveStatusTransformVar(statusTransformVar.v1, bindings, status, onStackAction);
      const v2 = resolveStatusTransformVar(statusTransformVar.v2, bindings, status, onStackAction);
      const val = v1 - v2;
      return Math.max(0, val) as any;
    }
    case "Static": {
      return statusTransformVar.a;
    }
    case "StatusOwner": {
      return status.owner as any;
    }
    case "StatusValue": {
      return status.value as any;
    }
    case "Var": {
      return bindings[statusTransformVar.bindingName];
    }
    case "IdOfStatus": {
      return status.id as any;
    }
  }
}