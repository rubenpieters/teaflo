import { FrUnit } from "src/shared/game/unit";
import { mkDamageI, mkStatic, mkFromInput } from "../../game/intent";
import { mkPositionId } from "../../game/entityId";
import { mkTargetInput } from "../../game/ability";

export const fr_unit_a1_l1_01: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: mkDamageI(
        mkFromInput(0),
        mkStatic(1),
      ),
      inputs: [
        mkTargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ]
}

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
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ]
}

export const frUnitMap: {
  [key: string]: FrUnit,
} = {
  // act 1 level 1
  "fr_unit_a1_l1_01": fr_unit_a1_l1_01,
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
}