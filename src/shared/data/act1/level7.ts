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
export const a1l7_fr1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l7_fr1: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 6,
  maxCharges: 6,
  abilities: [
    {
      ability: a1l7_fr1_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l7_fr1_ab1",
    },
  ],
  essential: true,
  cardId: "a1l7_fr1",
}

export const a1l7_fr2_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l7_fr2: FrUnit = {
  hp: 60,
  maxHp: 60,
  charges: 0,
  maxCharges: 6,
  abilities: [
    {
      ability: a1l7_fr2_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l7_fr2_ab1",
    },
  ],
  essential: true,
  cardId: "a1l7_fr2",
}

export const a1l7_fr3_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(3), new Ab.FromInput(0)),
    new A.Damage("Ability", "Target", new Ab.Static(5), new Ab.FromInput(0)),
  ]);

export const a1l7_fr3: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: a1l7_fr3_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l7_fr3_ab1",
    },
  ],
  essential: true,
  cardId: "a1l7_fr3",
}

export const a1l7_fr4_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(11), Ab.self(), Ab.allEnemy()),
  ]);

export const a1l7_fr4: FrUnit = {
  hp: 60,
  maxHp: 60,
  charges: 3,
  maxCharges: 3,
  abilities: [
    {
      ability: a1l7_fr4_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l7_fr4_ab1",
    },
  ],
  essential: true,
  cardId: "a1l7_fr4",
}

/**
 * En Unit
 */
export const a1l7_en1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(3), Ab.allFriendly()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l7_en1_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(12), Ab.highestThreat()),
  ]);

export const a1l7_en1: EnUnit = {
  hp: 60,
  maxHp: 60,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l7_en1_ab1,
      spriteId: "ab3",
      name: "a1l7_en2_ab1",
    },
    1: {
      ability: a1l7_en1_ab2,
      spriteId: "ab3",
      name: "a1l7_en2_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l7_en1",
}