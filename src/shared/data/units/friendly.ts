import { FrUnit } from "src/shared/game/unit";
import * as I from "../../game/intent";
import { PositionId } from "../../game/entityId";
import { TargetInput } from "../../game/ability";

export const fr_unit_a1_l1_01: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          new I.Static(new PositionId(0, "friendly")),
          new I.Static(1),
        ),
        new I.DamageI(
          new I.FromInput(0),
          new I.Static(1),
        ),
      ]),
      inputs: [
        new TargetInput(),
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
      intent: new I.DamageI(
        new I.FromInput(0),
        new I.Static(1),
      ),
      inputs: [
        new TargetInput(),
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