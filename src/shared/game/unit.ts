import { Ability } from "./ability";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
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