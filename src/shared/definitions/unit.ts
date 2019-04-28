import { Ability } from "./ability";
import { UserInput } from "./input";
import { AIPosition } from "./ai";
import { CardId } from "../data/cardId";


export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
  essential: boolean,
  cardId: CardId,
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
  abilities: { [K in number]: EnAbility },
  aiPosition: AIPosition,
}