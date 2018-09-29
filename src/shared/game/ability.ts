import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { GameState, CreatureId } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";
import { Next } from "src/shared/game/next";
import { SolCard } from "src/shared/game/solution";

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
      { effect: (_inputs: any[]) => { return (_state: GameState, _id: CreatureId) => {
        return action;
        }},
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
      { effect: (_state: GameState, _id: CreatureId) => {
        return action;
        },
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

// + additional user input
// entity effect = state, id, type -> state
// player effect = state -> state

// trigger : additional action which triggered

export type EntityEffect = {
  effect: (state: GameState, id: CreatureId) => Action,
  description: string,
  inputs: InputType[],
}

export type EnemyEffect = {
  effect: (state: GameState, id: CreatureId) => { action: Action, next: Next },
  description: string,
}

export type InputEntityEffect = {
  effect: (inputs: any[]) => (state: GameState, id: CreatureId) => Action,
  description: string,
  inputs: InputType[],
}

export type TriggerEntityEffect = {
  effect: (action: Action) => (state: GameState, id: CreatureId) => { action: Action, chargeUse: number },
  description: string,
  charges: number,
  type: "before" | "instead",
}
export type InstanceEffect = {
  effect: (state: GameState, id: CreatureId) => Action,
  description: string,
}