import { FrUnit, EnUnit } from "src/shared/game/unit";
import { ai1 } from "../ai/ai";
import { mkDamageI, mkStatic } from "../../game/intent";
import { mkPositionId } from "../../game/entityId";
import { mkTargetInput } from "../../game/ability";

export const unit1: FrUnit = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: mkDamageI(
        mkStatic(mkPositionId(0, "enemy")),
        mkStatic(1),
      ),
      inputs: [
        mkTargetInput(),
      ],
    }
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