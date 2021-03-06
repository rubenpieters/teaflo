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
    new A.UseCharge("Ability", "Target", new Ab.Static(9), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(9), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(9), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_i: FrUnit = {
  hp: 40,
  maxHp: 40,
  charges: 1,
  maxCharges: 50,
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
    new A.UseCharge("Ability", "Target", new Ab.Static(20), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(14), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(14), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_ii: FrUnit = {
  hp: 35,
  maxHp: 35,
  charges: 1,
  maxCharges: 50,
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
    new A.UseCharge("Ability", "Target", new Ab.Static(41), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(35), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(35), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_iii: FrUnit = {
  hp: 40,
  maxHp: 40,
  charges: 1,
  maxCharges: 80,
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
    new A.UseCharge("Ability", "Target", new Ab.Static(12), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(8), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(15), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(15), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_iv: FrUnit = {
  hp: 40,
  maxHp: 40,
  charges: 1,
  maxCharges: 60,
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

export const c_i_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(10), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(12), new Ab.FromInput(0)),
  ]);

export const c_i: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 40,
  maxCharges: 40,
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
    new A.UseCharge("Ability", "Target", new Ab.Static(10), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(20), new Ab.FromInput(0)),
    new A.Damage("Ability", "Target", new Ab.Static(5), new Ab.FromInput(0)),
  ]);

export const c_ii: FrUnit = {
  hp: 15,
  maxHp: 15,
  charges: 40,
  maxCharges: 40,
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
    new A.UseCharge("Ability", "Target", new Ab.Static(10), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(15), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(12), Ab.self(), Ab.allEnemy()),
  ]);

export const c_iii: FrUnit = {
  hp: 16,
  maxHp: 16,
  charges: 41,
  maxCharges: 41,
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

export const u_i_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(10), Ab.self()),
    new A.RemoveThreat("Ability", "Target", new Ab.Static(30), new Ab.FromInput(0), Ab.allEnemy()),
    new A.AddThreat("Ability", "Target", new Ab.Static(30), Ab.self(), Ab.allEnemy()),
  ]);

export const u_i: FrUnit = {
  hp: 24,
  maxHp: 24,
  charges: 30,
  maxCharges: 30,
  abilities: [
    {
      ability: u_i_ab1,
      inputs: [
        new FriendlyInput(),
      ],
      spriteId: "ab3",
      name: "u_i_ab1",
    },
  ],
  essential: true,
  cardId: "u_i",
}

export const u_ii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(20), Ab.self()),
    new A.Heal("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
  ]);

export const u_ii: FrUnit = {
  hp: 10,
  maxHp: 10,
  charges: 0,
  maxCharges: 20,
  abilities: [
    {
      ability: u_ii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "c_ii_ab1",
    },
  ],
  essential: true,
  cardId: "u_ii",
}

export const u_iii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(3), Ab.allFriendly()),
    new A.Damage("Ability", "Target", new Ab.Static(5), new Ab.FromInput(0)),
  ]);

export const u_iii: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 10,
  maxCharges: 10,
  abilities: [
    {
      ability: u_iii_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "u_iii_ab1",
    },
  ],
  essential: true,
  cardId: "u_iii",
}