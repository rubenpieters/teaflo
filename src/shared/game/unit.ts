import { Ability } from "./ability";
import { focus, over } from "../iassign-util";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
  essential: number,
}

export type FrAbility = {
  ability: Ability,
  inputs: any[],
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
  startingAbility: number,
}

export function useChargeUnit<E extends Unit>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.charges, x => x - value),
  );
}