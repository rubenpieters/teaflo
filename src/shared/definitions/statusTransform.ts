import { ConditionVar } from "./condition";
import { ActionWithOriginF, ActionWithOriginFG } from "./action";
import { StatusTransform_URI, SATarget_URI } from "./hkt";
import { ActionF } from "./actionf";

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

export class Var {
  public readonly tag: "Var" = "Var";

  constructor(
    public readonly bindingName: string,
  ) {}
}

export type StatusTransformVar<A>
  = ConditionVar<A>
  | Monus
  | Add
  | Var
  ;

export type StatusTransform = ActionWithOriginF<StatusTransform_URI>;

export type StatusAbilityWithOrigin = ActionWithOriginFG<StatusTransform_URI, SATarget_URI>;

export type StatusAbility = ActionF<StatusTransform_URI, SATarget_URI>;
