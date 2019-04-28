import { Ability } from "../../definitions/ability";
import * as Ab from "../../definitions/ability";
import * as A from "../../definitions/actionf";
import * as S from "../../definitions/status";
import { AIDirection } from "../../definitions/ai";
import { EnUnit, FrUnit } from "../../definitions/unit";
import { TargetInput, EnemyInput, FriendlyInput } from "../../definitions/input";

/**
 * Fr Unit
 */
export const a1l4_fr1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(4), Ab.allEnemy()),
    new A.AddThreat("Ability", "Target", new Ab.Static(4), Ab.self(),  Ab.allEnemy()),
  ]);

export const a1l4_fr1: FrUnit = {
  hp: 15,
  maxHp: 15,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l4_fr1_ab1,
      inputs: [
      ],
      spriteId: "ab3",
      name: "a1l4_fr1_ab1",
    },
  ],
  essential: true,
  cardId: "a1l4_fr1",
}

export const a1l4_fr2_ab1: Ability =
  new A.AddThreat("Ability", "Target", new Ab.Static(13), Ab.self(), Ab.allEnemy())
  ;

export const a1l4_fr2_ab2: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(3), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(20), new Ab.FromInput(0)),
  ]);

export const a1l4_fr2: FrUnit = {
  hp: 30,
  maxHp: 30,
  charges: 4,
  maxCharges: 6,
  abilities: [
    {
      ability: a1l4_fr2_ab1,
      inputs: [
      ],
      spriteId: "ab1",
      name: "a1l4_fr2_ab1",
    },
    {
      ability: a1l4_fr2_ab2,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab3",
      name: "a1l4_fr2_ab2",
    },
  ],
  essential: true,
  cardId: "a1l4_fr2",
}

/**
 * En Unit
 */
export const a1l4_en1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(7), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l4_en1_ab2: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l4_en1_ab3: Ability =
  new A.Damage("Ability", "Target", new Ab.Static(20), Ab.highestThreat())
  ;

export const a1l4_en1: EnUnit = {
  hp: 24,
  maxHp: 24,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l4_en1_ab1,
      spriteId: "ab3",
      name: "a1l4_en1_ab1",
    },
    1: {
      ability: a1l4_en1_ab2,
      spriteId: "ab3",
      name: "a1l4_en1_ab2",
    },
    2: {
      ability: a1l4_en1_ab3,
      spriteId: "ab3",
      name: "a1l4_en1_ab3",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l4_en1",
}

export const a1l4_en2_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(7), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l4_en2_ab2: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l4_en2_ab3: Ability =
  new A.Damage("Ability", "Target", new Ab.Static(20), Ab.highestThreat())
  ;

export const a1l4_en2: EnUnit = {
  hp: 24,
  maxHp: 24,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l4_en2_ab1,
      spriteId: "ab3",
      name: "a1l4_en2_ab1",
    },
    1: {
      ability: a1l4_en2_ab2,
      spriteId: "ab3",
      name: "a1l4_en2_ab2",
    },
    2: {
      ability: a1l4_en2_ab3,
      spriteId: "ab3",
      name: "a1l4_en2_ab3",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l4_en2",
}