import { Action } from "./action";
import { Intent, intentVarText } from "./intent";

export type HasAbilities = {
  abilities: Ability[],
}

export type Ability = {
  intent: Intent,
  inputs: UserInput[],
  spriteId: string,
};

type NumberInput = {
  tag: "NumberInput",
};

export function mkNumberInput(
): NumberInput {
  return {
    tag: "NumberInput",
  }
}

type TargetInput = {
  tag: "TargetInput",
};

export function mkTargetInput(
): TargetInput {
  return {
    tag: "TargetInput",
  }
}

export type UserInput
  = NumberInput
  | TargetInput
  ;

export function abilityText(
  ability: Ability,
) {
  switch (ability.intent.tag) {
    case "DamageI": {
      return `DMG ${intentVarText(ability.intent.value)} to ${intentVarText(ability.intent.target)}`;
    }
    case "HealI": {
      return `HEAL ${intentVarText(ability.intent.value)} to ${intentVarText(ability.intent.target)}`;
    }
  }
}