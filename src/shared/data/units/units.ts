import { FrUnit, EnUnit } from "src/shared/game/unit";
import { mkDamage } from "src/shared/game/action";
import { ai1 } from "../ai/ai";

export const unit1 = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  abilities: [
    mkDamage({ tag: "PositionId", type: "enemy", id: 0 }, 1),
  ]
}

export const unit2 = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  ai: ai1,
  currentAI: 0,
}

export const frUnitMap: {
  [key: string]: FrUnit,
} = {
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
}

export const enUnitMap: {
  [key: string]: EnUnit,
} = {
  "card1": unit2,
  "card2": unit2,
  "card3": unit2,
}