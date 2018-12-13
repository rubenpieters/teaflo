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

export class NumberInput {
  constructor(
    public readonly tag: "NumberInput" = "NumberInput",
  ) {}
}

export class TargetInput {
  constructor(
    public readonly tag: "TargetInput" = "TargetInput",
  ) {}
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
      return `USE ${intentVarText(intent.value)} CH to ${intentVarText(intent.target)}`;
    }
    case "CombinedIntent": {
      const texts = intent.intents.map(intentText);
      return texts.join(" && ");
    }
    case "AddThreatI": {
      return `ADD ${intentVarText(intent.value)} TH to ${intentVarText(intent.toFriendly)} at ${intentVarText(intent.atEnemy)}`
    }
  }
}