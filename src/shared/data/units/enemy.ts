import { EnUnit } from "../../game/unit";
import * as AI from "../ai/ai";
import * as T from "../../game/trigger";

export const en_unit_a1_l1_01: EnUnit = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  ai: AI.ai1,
  currentAI: 0,
}

export const en_unit_a1_l2_01: EnUnit = {
  hp: 24,
  maxHp: 24,
  charges: 5,
  maxCharges: 5,
  ai: AI.ai2,
  currentAI: 0,
}

export const en_unit_a2_l1_01: EnUnit = {
  hp: 45,
  maxHp: 45,
  charges: 5,
  maxCharges: 5,
  ai: AI.ai3,
  currentAI: 0,
}

export const en_unit_a1_l3_01: EnUnit = {
  hp: 45,
  maxHp: 45,
  charges: 5,
  maxCharges: 5,
  ai: AI.ai4,
  currentAI: 0,
}

export const enUnitMap: {
  [key: string]: EnUnit,
} = {
  // act 1 level 1
  "en_unit_a1_l1_01": en_unit_a1_l1_01,
  // act 1 level 2
  "en_unit_a1_l2_01": en_unit_a1_l2_01,
  // act 1 level 2
  "en_unit_a1_l3_01": en_unit_a1_l3_01,
  // act 2 level 1
  "en_unit_a2_l1_01": en_unit_a2_l1_01,
  "card1": en_unit_a1_l1_01,
  "card2": en_unit_a1_l1_01,
  "card3": en_unit_a1_l1_01,
}