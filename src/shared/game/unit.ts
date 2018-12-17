import { Ability, HasAbilities } from "./ability";
import { HasAI } from "./ai";
import { HasThreatMap } from "./threat";
import { Trigger } from "./trigger";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
  triggers: Trigger[],
}

export type FrUnit = Unit & HasAbilities
export type EnUnit = Unit & HasAI