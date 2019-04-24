import { Ability } from "../../definitions/ability";
import * as Ab from "../../definitions/ability";
import * as A from "../../definitions/actionf";
import * as S from "../../definitions/status";
import { AIDirection } from "../../definitions/ai";
import { EnUnit, FrUnit } from "../../definitions/unit";
import { TargetInput, EnemyInput } from "../../definitions/input";

/**
 * Fr Unit
 */
export const a1l1_fr_ab: Ability =
  new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0))
  ;

export const a1l1_fr: FrUnit = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l1_fr_ab,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab3",
      name: "a1l1_fr_ab",
    },
  ],
  essential: true,
  cardId: "a1l1_fr",
}

/**
 * En Unit
 */
export const a1l1_en_ab: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(5), Ab.highestThreat()),
  ]);

export const a1l1_en: EnUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l1_en_ab,
      spriteId: "ab3",
      name: "l1_en_ab",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l1_en",
}