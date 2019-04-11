import { ActionWithOriginF } from "./action";
import { Condition_URI } from "./hkt";
import { UnitId, StatusId } from "./entityId";

export class StatusCondition {
  public readonly tag: "StatusCondition" = "StatusCondition";

  constructor(
    public readonly actionCondition: ActionCondition,
  ) {}
}

export type Condition
  = StatusCondition
  ;

export type ActionCondition = ActionWithOriginF<Condition_URI>;

/**
 * A Condition Variable is a description of how a parameter of an action should be compared in a condition.
 */
export type ConditionVar<A>
  = Static<A>
  | Var
  | StatusOwner
  | StatusValue
  | IdOfStatus
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

export class IdOfStatus {
  public readonly tag: "IdOfStatus" = "IdOfStatus";
  
  constructor(

  ) {}
}

export function idOfStatus(): ConditionVar<StatusId> {
  return new IdOfStatus();
}