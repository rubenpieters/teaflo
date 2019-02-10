import { Action } from "./action";
import { Intent, intentVarText } from "./intent";
import { GlobalId } from "./entityId";

export type HasAbilities = {
  abilities: Ability[],
}

export type Ability = {
  intent: Intent,
  inputs: UserInput[],
  spriteId: string,
};

// user must specify a number
export class NumberInput {
  constructor(
    public readonly tag: "NumberInput" = "NumberInput",
  ) {}
}

// user must specify a target (a unit or a status)
export class TargetInput {
  constructor(
    public readonly tag: "TargetInput" = "TargetInput",
  ) {}
}

// user must specify a unit (friendly or enemy)
export class UnitInput {
  constructor(
    public readonly tag: "UnitInput" = "UnitInput",
  ) {}
}

// user must specify a status (friendly or enemy)
export class StatusInput {
  constructor(
    public readonly tag: "StatusInput" = "StatusInput",
  ) {}
}

export type UserInput
  = NumberInput
  | TargetInput
  | UnitInput
  | StatusInput
  ;

export function matchUserInput(
  userInput: UserInput,
  globalId: GlobalId<"friendly" | "enemy" | "status">,
): boolean {
  switch (userInput.tag) {
    case "NumberInput": {
      return false;
    }
    case "TargetInput": {
      return true;
    }
    case "UnitInput": {
      return globalId.type === "friendly" || globalId.type === "enemy";
    }
    case "StatusInput": {
      return globalId.type === "status";
    }
  }
}

export function abilityText(
  ability: Ability,
): string {
  return intentText(ability.intent);
}

export function intentText(
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
    case "AddTriggerI": {
      return `STA ${intentVarText(intent.trigger)} to ${intentVarText(intent.target)}`;
    }
    case "SwapHPWithExcessI": {
      return `SWAP HP with ${intentVarText(intent.target1)} and ${intentVarText(intent.target2)}, excess is armor`;
    }
  }
}