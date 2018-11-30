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

type TargetInput = {
  tag: "TargetInput",
}

export type UserInput
  = NumberInput
  | TargetInput
  ;