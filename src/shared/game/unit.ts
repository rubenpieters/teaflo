import { Ability, HasAbilities } from "./ability";
import { HasAI } from "./ai";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
}

export type FrUnit = Unit & HasAbilities
export type EnUnit = Unit & HasAI