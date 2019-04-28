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
export const a1l3_fr1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l3_fr1: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l3_fr1_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l3_fr1_ab1",
    },
  ],
  essential: true,
  cardId: "a1l3_fr1",
}

export const a1l3_fr2_ab1: Ability =
  new A.AddThreat("Ability", "Target", new Ab.Static(13), Ab.self(), new Ab.FromInput(0))
  ;

export const a1l3_fr2: FrUnit = {
  hp: 50,
  maxHp: 50,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l3_fr2_ab1,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab1",
      name: "a1l3_fr2_ab1",
    },
  ],
  essential: true,
  cardId: "a1l3_fr2",
}

/**
 * En Unit
 */
export const a1l3_en1_ab1: Ability =
  new A.Damage("Ability", "Target", new Ab.Static(7), Ab.highestThreat())
  ;

export const a1l3_en1: EnUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l3_en1_ab1,
      spriteId: "ab3",
      name: "a1l3_en1_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l3_en1",
}

export const a1l3_en2_ab1: Ability =
  new A.Damage("Ability", "Target", new Ab.Static(7), Ab.highestThreat())
  ;

export const a1l3_en2: EnUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l3_en2_ab1,
      spriteId: "ab3",
      name: "a1l3_en2_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l3_en2",
}