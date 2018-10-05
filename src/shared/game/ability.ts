import { focus, over, set } from "src/shared/iassign-util";
import { TargetType } from "src/shared/game/target";
import { Action } from "src/shared/game/action";
import { GameState, CreatureId } from "src/shared/game/state";
import { Card } from "src/shared/game/card";
import { InputType } from "src/shared/game/input";
import { Next } from "src/shared/game/next";
import { SolCard } from "src/shared/game/solution";
import { Context } from "./effectvar";

export function solCardFromAbility(
  effect: InputEntityEffect,
  id: CreatureId,
): Card {
  return {
    name: "-- created --",
    effects: [
      effect
    ],
    tag: "general",
    origin: id,
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