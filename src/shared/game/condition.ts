import { UnitId, StatusId } from "../definitions/entityId";
import deepEqual from "deep-equal";
import { StStatus } from "../definitions/statusRow";
import { Action } from "../definitions/action";
import { ActionCondition, ConditionVar } from "../definitions/condition";

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
    case "IdOfStatus": {
      const result = deepEqual(val, status.id);
      return { result };
    }
  }
}
