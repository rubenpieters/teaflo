import { Consume, Resource } from "src/shared/resourceType";

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

export type NodeEffect = NilEffect | GainEffect | ClearTemp | ConsumeEffect;