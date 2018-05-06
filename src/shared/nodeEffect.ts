import { Consume, Resource } from "src/shared/resourceType";
import { Modifier } from "src/shared/modifier";
import { ModifierEffect } from "src/shared/modifierEffect";

export type GainEffect = {
  tag: "GainEffect",
  gains: Resource[],
}

export type ClearTemp = {
  tag: "ClearTemp",
}

export type NilEffect = {
  tag: "NilEffect",
}

export type ConsumeEffect = {
  tag: "ConsumeEffect",
  consume: Consume[],
  afterConsume: NodeEffect[],
}

export type PersistEffect = {
  tag: "PersistEffect",
}

export type AddModifier = {
  tag: "AddModifier",
  modifierType: Modifier,
}

export type NodeEffect
  = NilEffect
  | GainEffect
  | ClearTemp
  | ConsumeEffect
  | PersistEffect
  | AddModifier
  ;