import { focus, over, set } from "src/shared/iassign-util";
import { Ability, HasAbilities } from "./ability";
import { HasAI } from "./ai";
import { HasThreatMap } from "./threat";
import { Trigger, HasTriggers } from "./trigger";

export type Unit = {
  hp: number,
  maxHp: number,
  charges: number,
  maxCharges: number,
} & HasTriggers;

export type FrUnit = Unit & HasAbilities & { vital: boolean };
export type EnUnit = Unit & HasAI;

export function damage<U extends Unit>(
  u: U,
  value: number,
): U {
  return focus(u, over(x => x.hp, x => {
    const newVal = x - value;
    return newVal < 0 ? 0 : newVal;
  }));
}

export function heal<U extends Unit>(
  u: U,
  value: number,
): U {
  return focus(u, over(x => x.hp, x => {
    const newVal = x + value;
    return newVal > u.maxHp ? u.maxHp : newVal;
  }));
}

export function useCharge<U extends Unit>(
  u: U,
  value: number,
): U {
  return focus(u, over(x => x.charges, x => {
    const newVal = x - value;
    return newVal < 0 ? 0 : newVal;
  }));
}