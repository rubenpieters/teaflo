import { focus, over, set } from "../iassign-util";
import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { UnitId, TargetId } from "./entityId";
import { GameState } from "./state";
import { damageEntity } from "./entity";
import { useChargeUnit } from "./unit";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Action: A,
  }
}

export type ACTION_URIS = Ability_URI | Action_URI | Target_URI;

export const Ability_URI: "Ability" = "Ability";
export type Ability_URI = typeof Ability_URI;

export const Action_URI: "Action" = "Action";
export type Action_URI = typeof Action_URI;

export const Target_URI: "Target" = "Target";
export type Target_URI = typeof Target_URI;

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

export class Death<G extends URIS> {
  public readonly tag: "Death" = "Death";

  constructor(
    readonly uriG: G,
    readonly target: Type<G, UnitId>,
  ) {}
}

export class Combined<F extends URIS, G extends URIS> {
  public readonly tag: "Combined" = "Combined";

  constructor(
    public readonly list: ActionF<F, G>[],
  ) {}
}

export function CombinedAction(
  list: ActionF<"Action", "Action">[],
): Combined<"Action", "Action"> {
  return new Combined(list);
}

export type ActionF<F extends URIS, G extends URIS>
  = Damage<F, G>
  | UseCharge<F, G>
  | Invalid
  | Death<G>
  | Combined<F, G>
  ;

export const actionTags: Action["tag"][]
  = [ "Damage",
      "UseCharge",
    ]
  ;

/**
 * An Action describes a single step in gamestate transitions.
 */
export type Action
  = ActionF<Action_URI, Action_URI>
  ;

export function applyAction(
  state: GameState,
  action: Action,
): { state: GameState, actions: Action[] } {
  switch (action.tag) {
    case "Damage": {
      const result = state.overTarget(
        action.target,
        x => damageEntity(x, action.value),
      );

      const entity = result.entity;
      let actions: Action[] = [];
      if (entity !== undefined && entity.hp <= 0) {
        actions = [new Invalid()];
      }
      return { state: result.state, actions };
    }
    case "UseCharge": {
      const result = state.overTarget(
        action.target,
        x => useChargeUnit(x, action.value),
      );

      const entity = result.entity;
      let actions: Action[] = [];
      if (entity !== undefined && entity.charges <= 0) {
        actions = [new Invalid()];
      }
      return { state: result.state, actions };
    }
    case "Invalid": {
      throw "unimplemented";
    }
    case "Death": {
      const result = state.removeTarget(action.target);
      const entity = result.entity;
      
      let actions: Action[] = [];
      if (entity !== undefined && entity.essential) {
        actions = [new Invalid()];
      }

      return { state: result.state, actions };
    }
    case "Combined": {
     return { state, actions: action.list };
    }
  }
}