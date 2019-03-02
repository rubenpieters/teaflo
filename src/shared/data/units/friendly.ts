import { FrUnit } from "../../game/unit";
import * as I from "../../game/intent";
import { TargetInput, StatusInput, UnitInput } from "../../game/ability";
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
      spriteId: "ab3",
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
      spriteId: "ab1",
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
          new I.Static(T.full(new T.Armor(10))),
        ),
      ]),
      inputs: [
        new UnitInput(),
      ],
      spriteId: "ab2",
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
          new I.Static(T.full(new T.Weak(1))),
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
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(5),
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
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(2),
        ),
        new I.SwapHPWithExcessI(
          new I.FromInput(0),
          new I.FromInput(1),
        ),
      ]),
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
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(4),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.AllyWeakSelfArmor(3))),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(7),
        ),
        new I.AddTriggerI(
          new I.AllExceptSelf(),
          new I.Static(T.full(new T.Weak(2))),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
  ],
  vital: true,
};

export const fr_unit_a2_03: FrUnit = {
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(8),
        ),
        new I.DamageI(
          new I.FromInput(0),
          new I.Static(7),
        ),
        I.thDamage(
          new I.FromInput(1),
          new I.Static(10),
        ),
      ]),
      inputs: [
        new StatusInput(),
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(3),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.Weak(1))),
        ),
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(T.full(new T.Strong(2))),
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

export const fr_unit_a2_04: FrUnit = {
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(5),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.Explode(10, 10))),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(3),
        ),
        new I.DamageI(
          new I.FromInput(0),
          new I.Static(10),
        ),
        new I.AddTriggerI(
          new I.FromInput(1),
          new I.Static(T.full(new T.Armor(5))),
        ),
      ]),
      inputs: [
        new StatusInput(),
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
  ],
  vital: true,
};

export const fr_unit_a2_05: FrUnit = {
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(9),
        ),
        new I.DamageI(
          new I.FromInput(0),
          new I.Static(5),
        ),
        new I.DamageI(
          new I.FromInput(1),
          new I.Static(5),
        ),
        new I.HealI(
          new I.FromInput(2),
          new I.Static(10),
        ),
      ]),
      inputs: [
        new StatusInput(),
        new StatusInput(),
        new StatusInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(5),
        ),
        new I.DamageI(
          new I.FromInput(0),
          new I.Static(16),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.Weak(1))),
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

export const fr_unit_a2_06: FrUnit = {
  hp: 70,
  maxHp: 70,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(2),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.ThreatOnAllyDamage(2))),
        ),
      ]),
      inputs: [
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(5),
        ),
        new I.AddTriggerI(
          I.mkSelf(),
          new I.Static(T.full(new T.Armor(14))),
        ),
        new I.DamageI(
          I.mkAllyExceptSelf(),
          new I.Static(3),
        ),
      ]),
      inputs: [
      ],
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
};

export const test4_ab: FrUnit = {
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
          new I.Static(20),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab1",
    },
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
      spriteId: "ab2",
    },
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
      spriteId: "ab3",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        I.thDamage(
          new I.FromInput(0),
          new I.Static(20),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab4",
    },
  ],
  vital: true,
};

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
  "fr_unit_a2_04": fr_unit_a2_04,
  "fr_unit_a2_05": fr_unit_a2_05,
  "fr_unit_a2_06": fr_unit_a2_06,
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
  "test4ab": test4_ab,
}