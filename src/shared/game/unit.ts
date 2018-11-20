import { Ability } from "./ability";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
  abilities: Ability[],
}