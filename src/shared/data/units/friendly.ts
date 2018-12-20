import { FrUnit } from "src/shared/game/unit";
import * as I from "../../game/intent";
import { PositionId } from "../../game/entityId";
import { TargetInput } from "../../game/ability";
import * as T from "../../game/trigger";

export const fr_unit_a1_l1_01: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        I.thDamage(
          new I.FromInput(0),
          new I.Static(5),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ],
  triggers: [],
};

export const fr_unit_a1_l2_01: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        I.thDamage(
          new I.FromInput(0),
          new I.Static(10),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ],
  triggers: [],
};

export const fr_unit_a1_l2_02: FrUnit = {
  hp: 11,
  maxHp: 11,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        new I.AddThreatI(
          I.mkSelf(),
          I.mkAllEnemy(),
          new I.Static(13),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ],
  triggers: [],
};

export const fr_unit_a1_l2_03: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(new T.Armor(1000)),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ],
  triggers: [],
};

export const unit1: FrUnit = {
  hp: 100,
  maxHp: 100,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: I.thDamage(
        new I.FromInput(0),
        new I.Static(1),
      ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    }
  ],
  triggers: [],
}

export const frUnitMap: {
  [key: string]: FrUnit,
} = {
  // act 1 level 1
  "fr_unit_a1_l1_01": fr_unit_a1_l1_01,
  // act 1 level 2
  "fr_unit_a1_l2_01": fr_unit_a1_l2_01,
  "fr_unit_a1_l2_02": fr_unit_a1_l2_02,
  "fr_unit_a1_l2_03": fr_unit_a1_l2_03,
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
}