import { URIS, Type } from "fp-ts/lib/HKT";
import { TargetId, UnitId, EnemyId, FriendlyId } from "./entityId";
import { AIDirection } from "./ai";
import { Status } from "./status";
import { ActionWithOrigin } from "./action";

/**
 * A generic shape of actions.
 */
export class Damage<F extends URIS, G extends URIS> {
  public readonly tag: "Damage" = "Damage";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    readonly value: Type<F, number>,
    readonly target: Type<G, TargetId>,
  ) {}
}

export class UseCharge<F extends URIS, G extends URIS> {
  public readonly tag: "UseCharge" = "UseCharge";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    readonly value: Type<F, number>,
    readonly target: Type<G, UnitId>,
  ) {}
}

export class Invalid {
  public readonly tag: "Invalid" = "Invalid";

  constructor(
  ) {}
}

export const invalidNoOrigin: ActionWithOrigin =
  {...new Invalid, origin: "noOrigin" };

export class Death<G extends URIS> {
  public readonly tag: "Death" = "Death";

  constructor(
    readonly uriG: G,
    readonly target: Type<G, TargetId>,
  ) {}
}

export class Combined<F extends URIS, G extends URIS> {
  public readonly tag: "Combined" = "Combined";

  constructor(
    public readonly list: ActionF<F, G>[],
  ) {}
}

export function combinedAction(
  list: ActionF<"Action", "Action">[],
): Combined<"Action", "Action"> {
  return new Combined(list);
}

export function combinedAbility(
  list: ActionF<"Ability", "Target">[],
): Combined<"Ability", "Target"> {
  return new Combined(list);
}

export class MoveAI<F extends URIS, G extends URIS> {
  public readonly tag: "MoveAI" = "MoveAI";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly dir: Type<F, AIDirection>,
    public readonly target: Type<G, EnemyId>,
  ) {}
}

export class AddThreat<F extends URIS, G extends URIS> {
  public readonly tag: "AddThreat" = "AddThreat";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly value: Type<F, number>,
    public readonly forAlly: Type<G, FriendlyId>,
    public readonly atEnemy: Type<G, EnemyId>,
  ) {}
}

export class AddStatus<F extends URIS, G extends URIS> {
  public readonly tag: "AddStatus" = "AddStatus";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly status: Type<F, Status>,
    public readonly target: Type<G, UnitId>,
  ) {}
}

export class StartTurn {
  public readonly tag: "StartTurn" = "StartTurn";

  constructor(
  ) {}
}

export type ActionF<F extends URIS, G extends URIS>
  = Damage<F, G>
  | UseCharge<F, G>
  | Invalid
  | Death<G>
  | Combined<F, G>
  | MoveAI<F, G>
  | AddThreat<F, G>
  | AddStatus<F, G>
  | StartTurn
  ;

export type ActionTag = ActionF<any, any>["tag"];