import { FrUnit } from "src/shared/game/unit";
import * as I from "../../game/intent";
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
    },
  ],
  vital: true,
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
    },
  ],
  vital: true,
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
      spriteId: "fr_unit_a1_l2_01_ab1",
    },
  ],
  vital: true,
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
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
  ],
  vital: true,
};

export const fr_unit_a1_l3_01: FrUnit = {
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
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(new T.Weak(100)),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    },
  ],
  vital: true,
};

export const fr_unit_a2_01: FrUnit = {
  hp: 40,
  maxHp: 40,
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
          I.mkAllExceptSelf(),
          new I.Static(10),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab1",
    },
    {
      intent: new I.SwapHPWithExcessI(
        new I.FromInput(0),
        new I.FromInput(1),
      ),
      inputs: [
        new TargetInput(),
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
    },
  ],
  vital: true,
};

export const fr_unit_a2_02: FrUnit = {
  hp: 25,
  maxHp: 25,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(2),
        ),
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(new T.Strong(500)),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(2),
        ),
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(new T.Strong(500)),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
  ],
  vital: true,
};

export const fr_unit_a2_03: FrUnit = {
  hp: 40,
  maxHp: 40,
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
          I.mkSelf(),
          new I.Static(new T.AllyWeakSelfArmor(300)),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        new I.AddTriggerI(
          new I.AllExceptSelf(),
          new I.Static(new T.Weak(200)),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
  ],
  vital: true,
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
    },
  ],
  vital: true,
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
  // act 1 level 3
  "fr_unit_a1_l3_01": fr_unit_a1_l3_01,
  // act 2
  "fr_unit_a2_01": fr_unit_a2_01,
  "fr_unit_a2_02": fr_unit_a2_02,
  "fr_unit_a2_03": fr_unit_a2_03,
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
}