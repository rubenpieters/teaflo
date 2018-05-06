import { ModifierEffect } from "./modifierEffect";

export type OneTimeModifier = {
  tag: "OneTimeModifier",
  effect: ModifierEffect,
}

export type ConstantModifier = {
  tag: "ConstantModifier",
  effect: ModifierEffect,
}


export type Modifier = OneTimeModifier | ConstantModifier;