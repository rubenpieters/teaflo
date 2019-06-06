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
  hp: 30,
  maxHp: 30,
  charges: 6,
  maxCharges: 6,
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

export const d_ii: FrUnit = {
  hp: 60,
  maxHp: 60,
  charges: 0,
  maxCharges: 6,
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
  cardId: "d_ii",
}

export const d_iii_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(2), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(20), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(20), Ab.self(), new Ab.FromInput(0)),
  ]);

export const d_iii: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 6,
  maxCharges: 6,
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

export const d_iv: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 0,
  maxCharges: 6,
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
  cardId: "d_iv",
}

export const c_i_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(2), new Ab.FromInput(0)),
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
    new A.RestoreCharge("Ability", "Target", new Ab.Static(3), new Ab.FromInput(0)),
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
    new A.RestoreCharge("Ability", "Target", new Ab.Static(3), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(20), Ab.self(), Ab.allEnemy()),
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