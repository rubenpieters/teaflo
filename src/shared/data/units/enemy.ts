import { EnUnit } from "src/shared/game/unit";
import { ai1 } from "../ai/ai";
import * as I from "../../game/intent";
import { PositionId } from "../../game/entityId";

export const en_unit_a1_l1_01: EnUnit = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  ai: ai1,
  currentAI: 0,
  triggers: [],
}

export const enUnitMap: {
  [key: string]: EnUnit,
} = {
  // act 1 level 1
  "en_unit_a1_l1_01": en_unit_a1_l1_01,
  "card1": en_unit_a1_l1_01,
  "card2": en_unit_a1_l1_01,
  "card3": en_unit_a1_l1_01,
}