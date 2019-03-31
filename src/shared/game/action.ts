import { focus, over, set } from "../iassign-util";
import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { UnitId, TargetId } from "./entityId";
import { GameState } from "./state";
import { damageEntity } from "./entity";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Action: A,
  }
}

type ACTION_URIS = Ability_URI | Action_URI;

export const Ability_URI: "Ability" = "Ability";
export type Ability_URI = typeof Ability_URI;

export const Action_URI: "Action" = "Action";
export type Action_URI = typeof Action_URI;

/**
 * A generic shape of actions.
 */
export class Damage<F extends URIS> {
  public readonly tag: "Damage" = "Damage";

  constructor(
    readonly uri: F,
    readonly value: Type<F, number>,
    readonly target: Type<F, TargetId>,
  ) {}
}

export class UseCharge<F extends URIS> {
  public readonly tag: "UseCharge" = "UseCharge";

  constructor(
    readonly uri: F,
    readonly value: Type<F, number>,
    readonly target: Type<F, UnitId>,
  ) {}
}

export type ActionF<F extends URIS>
  = Damage<F>
  | UseCharge<F>
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
  = ActionF<Action_URI>
  ;

export function applyAction(
  state: GameState,
  action: Action,
) {
  switch (action.tag) {
    case "Damage": {
      const newState = state.overTarget(
        action.target,
        x => damageEntity(x, action.value),
      );
      return { state: newState, actions: [] };
    }
    case "UseCharge": {
      return;
    }
  }
}