import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { GameState, CreatureId } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";
import { Next } from "src/shared/game/next";
import { SolCard } from "src/shared/game/solution";
import { Context } from "./effectvar";

// TODO: check with gamestate.ts whether given input is self global id or self position index
// (it seems to be position index)

export function createCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): Card {
  return {
    name: "-- created --",
    effects: [
      { effect: (_obj) => { return { action }; },
        inputs: [],
        description: "-- created --",
      },
    ],
    tag: "general",
    origin: {
      tag: "EntityOrigin", 
      entityId: selfId,
      entityType: selfType,
    }
  };
}

export function createSolCard(
  action: Action,
  selfId: number,
  selfType: TargetType,
): SolCard {
  return {
    name: "-- created --",
    effects: [
      { effect: (_obj) => { return { action }; },
        description: "-- created --",
      },
    ],
    tag: "general",
    origin: {
      tag: "EntityOrigin", 
      entityId: selfId,
      entityType: selfType,
    }
  };
}

// + additional user input
// entity effect = state, id, type -> state
// player effect = state -> state

// trigger : additional action which triggered

export type EntityEffect = {
  effect: (obj: Context) => { action: Action },
  description: string,
}

export type EnemyEffect = {
  effect: (obj: Context) => { action: Action, next: Next },
  description: string,
}

export type InputEntityEffect = {
  effect: (obj: Context) => { action: Action },
  description: string,
  inputs: InputType[],
}

export type TriggerEntityEffect = {
  effect: (obj: Context) => { action: Action, chargeUse: number },
  description: string,
  type: "before" | "instead",
}