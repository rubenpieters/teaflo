import { Action } from "./action";
import { Intent } from "./intent";

export type HasAbilities = {
  abilities: Ability[],
}

export type Ability = {
  intent: Intent,
  inputs: UserInput[],
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