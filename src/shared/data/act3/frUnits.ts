import { Ability } from "../../definitions/ability";
import * as Ab from "../../definitions/ability";
import * as A from "../../definitions/actionf";
import * as S from "../../definitions/status";
import { AIDirection } from "../../definitions/ai";
import { EnUnit, FrUnit } from "../../definitions/unit";
import { TargetInput, EnemyInput, FriendlyInput } from "../../definitions/input";
import * as C from "../../definitions/condition";

/**
 * Fr Unit
 */
export const d_i_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_i: FrUnit = {
  hp: 35,
  maxHp: 35,
  charges: 0,
  maxCharges: 3,
  abilities: [
    {
      ability: d_i_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "d_i_ab1",
    },
  ],
  essential: true,
  cardId: "d_i",
}


export const d_ii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(2), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(12), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(12), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_ii: FrUnit = {
  hp: 35,
  maxHp: 35,
  charges: 0,
  maxCharges: 3,
  abilities: [
    {
      ability: d_ii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "d_ii_ab1",
    },
  ],
  essential: true,
  cardId: "d_ii",
}

export const d_iii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(8), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(50), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(50), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_iii: FrUnit = {
  hp: 80,
  maxHp: 80,
  charges: 0,
  maxCharges: 8,
  abilities: [
    {
      ability: d_iii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "d_iii_ab1",
    },
  ],
  essential: true,
  cardId: "d_iii",
}

export const d_iv_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(5), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(15), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(15), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_iv: FrUnit = {
  hp: 60,
  maxHp: 60,
  charges: 0,
  maxCharges: 6,
  abilities: [
    {
      ability: d_iv_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "d_iv_ab1",
    },
  ],
  essential: true,
  cardId: "d_iv",
}

export const d_v_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(5), Ab.self()),
  ]);

export const d_v: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 0,
  maxCharges: 6,
  abilities: [
    {
      ability: d_iv_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "d_v_ab1",
    },
  ],
  essential: true,
  cardId: "d_v",
}

export const c_i_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), new Ab.FromInput(0)),
  ]);

export const c_i: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: c_i_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_i_ab1",
    },
  ],
  essential: true,
  cardId: "c_i",
}

export const c_ii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(2), new Ab.FromInput(0)),
    new A.Damage("Ability", "Target", new Ab.Static(5), new Ab.FromInput(0)),
  ]);

export const c_ii: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: c_ii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_ii_ab1",
    },
  ],
  essential: true,
  cardId: "c_ii",
}

export const c_iii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(2), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(15), Ab.self(), Ab.allEnemy()),
  ]);

export const c_iii: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: c_iii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_iii_ab1",
    },
  ],
  essential: true,
  cardId: "c_iii",
}

export const c_iv_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(2), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(15), Ab.self(), Ab.allEnemy()),
  ]);

export const c_iv: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: c_iv_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_iv_ab1",
    },
  ],
  essential: true,
  cardId: "c_iv",
}

export const c_v_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RemoveThreat("Ability", "Target", new Ab.Static(50), Ab.self(), Ab.allEnemy()),
  ]);

export const c_v: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: c_v_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_v_ab1",
    },
  ],
  essential: true,
  cardId: "c_v",
}