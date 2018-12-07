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
): string {
  return intentText(ability.intent);
}

function intentText(
  intent: Intent
): string {
  switch (intent.tag) {
    case "DamageI": {
      return `DMG ${intentVarText(intent.value)} to ${intentVarText(intent.target)}`;
    }
    case "HealI": {
      return `HEAL ${intentVarText(intent.value)} to ${intentVarText(intent.target)}`;
    }
    case "UseChargeI": {
      return `USE CH ${intentVarText(intent.value)} to ${intentVarText(intent.target)}`;
    }
    case "CombinedIntent": {
      const texts = intent.intents.map(intentText);
      return texts.join(" && ");
    }
  }
}