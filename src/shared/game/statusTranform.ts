import { hoistActionWithOriginF } from "./action";
import { StStatus } from "../definitions/statusRow";
import { StatusTransform, StatusTransformVar } from "../definitions/statusTransform";
import { ActionWithOrigin } from "../definitions/action";

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
    case "Trivial": {
      throw "resolveStatusTransformVar: unexpected statusTransform 'Trivial'";
    }
  }
}