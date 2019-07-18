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
export const a1l6_fr1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(10), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(15), new Ab.FromInput(0)),
  ]);

export const a1l6_fr1: FrUnit = {
  hp: 15,
  maxHp: 15,
  charges: 60,
  maxCharges: 60,
  abilities: [
    {
      ability: a1l6_fr1_ab1,
      inputs: [
        new FriendlyInput(),
      ],
      spriteId: "ab3",
      name: "a1l6_fr1_ab1",
    },
  ],
  essential: true,
  cardId: "a1l6_fr1",
}

export const a1l6_fr2_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(15), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.Heal("Ability", "Target", new Ab.Static(7), new Ab.Self()),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l6_fr2: FrUnit = {
  hp: 45,
  maxHp: 45,
  charges: 0,
  maxCharges: 60,
  abilities: [
    {
      ability: a1l6_fr2_ab1,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab1",
      name: "a1l6_fr2_ab1",
    },
  ],
  essential: true,
  cardId: "a1l6_fr2",
}

export const a1l6_fr3_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(20), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(18), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(18), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l6_fr3: FrUnit = {
  hp: 53,
  maxHp: 53,
  charges: 0,
  maxCharges: 60,
  abilities: [
    {
      ability: a1l6_fr3_ab1,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab1",
      name: "a1l6_fr3_ab1",
    },
  ],
  essential: true,
  cardId: "a1l6_fr2",
}

/**
 * En Unit
 */

export const a1l6_en1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(7), Ab.highestThreat()),
  ]);

export const a1l6_en1: EnUnit = {
  hp: 46,
  maxHp: 46,
  charges: 0,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l6_en1_ab1,
      spriteId: "ab3",
      name: "a1l6_en1_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l6_en1",
}

export const a1l6_en2_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(5), Ab.highestThreat()),
  ]);

export const a1l6_en2: EnUnit = {
  hp: 28,
  maxHp: 28,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l6_en2_ab1,
      spriteId: "ab3",
      name: "a1l6_en2_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l6_en2",
}