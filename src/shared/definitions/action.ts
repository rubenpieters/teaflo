import { Action_URI } from "./hkt";
import { ActionF } from "./actionf";
import { URIS, Type } from "fp-ts/lib/HKT";
import { UnitId } from "./entityId";

/**
 * An Action describes a single step in gamestate transitions.
 */
export type Action
  = ActionF<Action_URI, Action_URI>
  ;

export type ActionWithOriginFG<F extends URIS, G extends URIS>
  = ActionF<F, G>
  & { origin: Type<F, UnitId | "noOrigin"> }
  ;

export type ActionWithOriginF<F extends URIS>
  = ActionF<F, F>
  & { origin: Type<F, UnitId | "noOrigin"> }
  ;

export type ActionWithOrigin = ActionWithOriginF<"Action">;