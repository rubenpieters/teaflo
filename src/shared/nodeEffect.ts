import { Resource } from "src/shared/resourceType";

type GainEffect = {
  tag: "GainEffect",
  gains: Resource[],
}

type NilEffect = {
  tag: "NilEffect",
}

export type NodeEffect = NilEffect | GainEffect;