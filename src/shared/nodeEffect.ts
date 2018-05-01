import { Resource } from "src/shared/resourceType";

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

export type NodeEffect = NilEffect | GainEffect | ClearTemp;