import { FrUnit } from "../../game/unit";
import * as I from "../../game/intent";
import { TargetInput, StatusInput, UnitInput } from "../../game/ability";
import * as T from "../../game/trigger";
import { isTrue, Equal } from "../../type-util";
import { thDamage, cost } from "../intents/basic";
import { trinity_dmg, trinity_tank, trinity_support } from "../frunits/1_trinity";
import { dmg_1, armor_1 } from "../frunits/0_experiments";

export const fr_unit_a1_l1_01: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: cost(1,
        thDamage(
          new I.FromInput(0),
          new I.Static(5),
        ),
      ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
      name: "dmg5",
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
      intent: cost(1,
        thDamage(
          new I.FromInput(0),
          new I.Static(5),
        ),
      ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
      name: "dmg5",
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
      name: "weak1",
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
        thDamage(
          I.mkAllExceptSelf(),
          new I.Static(10),
        ),
      ]),
      inputs: [],
      spriteId: "fr_unit_a1_l1_01_ab1",
      name: "dmg10_allExceptSelf",
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
      name: "swapHP",
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
      name: "addTr_allyWeakSelfArmor3",
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
      name: "add_weak2",
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
        thDamage(
          new I.FromInput(1),
          new I.Static(10),
        ),
      ]),
      inputs: [
        new StatusInput(),
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab2",
      name: "dmg7_add10th",
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
      name: "add_weak1_strong2",
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
      name: "add_explode10/10",
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
      name: "dmg_status_add_armor",
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
      name: "dmg_2status_heal_status",
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
      name: "dmg16_self_weak",
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
      name: "add_threat_on_ally_damage",
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
      name: "self_armor_dmg_allyExceptSelf",
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
      intent: thDamage(
        new I.FromInput(0),
        new I.Static(1),
      ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "fr_unit_a1_l1_01_ab1",
      name: "dmg1",
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
        thDamage(
          new I.FromInput(0),
          new I.Static(20),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab1",
      name: "ab1",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        thDamage(
          new I.FromInput(0),
          new I.Static(5),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab2",
      name: "ab2",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        thDamage(
          new I.FromInput(0),
          new I.Static(5),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "ab3",
    },
    {
      intent: new I.CombinedIntent([
        new I.UseChargeI(
          I.mkSelf(),
          new I.Static(1),
        ),
        thDamage(
          new I.FromInput(0),
          new I.Static(20),
        ),
      ]),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab4",
      name: "ab4",
    },
  ],
  vital: true,
};


// check that values of frUnitMap are all `FrUnit`
type FrUnitMapValues = (typeof frUnitMap)[keyof (typeof frUnitMap)];
isTrue<Equal<FrUnitMapValues, FrUnit>>(true);

export const frUnitMap = {
  "dmg1": dmg_1,
  "arm1": armor_1,
  // act 1 level 1
  "fr_unit_a1_l1_01": fr_unit_a1_l1_01,
  // act 1 level 2
  "fr_unit_a1_l2_01": trinity_dmg,
  "fr_unit_a1_l2_02": trinity_tank,
  "fr_unit_a1_l2_03": trinity_support,
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

export type FrUnitId = keyof (typeof frUnitMap);