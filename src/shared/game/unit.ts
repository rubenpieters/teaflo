import { Ability } from "./ability";
import { focus, over } from "../iassign-util";
import { AIPosition, AIDirection, moveAI } from "./ai";
import { UserInput } from "./input";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
  essential: boolean,
}

export type FrAbility = {
  ability: Ability,
  inputs: UserInput[],
  name: string,
  spriteId: string,
}

export type FrUnit = Unit & {
  abilities: FrAbility[],
}

export type EnAbility = {
  ability: Ability,
  name: string,
  spriteId: string,
}

export type EnUnit = Unit & {
  abilities: EnAbility[],
  aiPosition: AIPosition,
}

export function useChargeUnit<E extends Unit>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.charges, x => x - value),
  );
}

export function moveAIUnit<E extends EnUnit>(
  e: E,
  dir: AIDirection,
): E {
  return focus(e,
    over(x => x.aiPosition, x => moveAI(x, dir)),
  );
}