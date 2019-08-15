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
export const a1l2_fr_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(9), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(9), new Ab.FromInput(0)),
  ]);

export const a1l2_fr: FrUnit = {
  hp: 40,
  maxHp: 40,
  charges: 1,
  maxCharges: 50,
  abilities: [
    {
      ability: a1l2_fr_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l2_fr_ab1",
    },
  ],
  essential: true,
  cardId: "a1l2_fr",
}

/**
 * En Unit
 */
export const a1l2_en_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(5), Ab.highestThreat()),
  ]);

export const a1l2_en: EnUnit = {
  hp: 15,
  maxHp: 15,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l2_en_ab1,
      spriteId: "ab3",
      name: "a1l2_en_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l2_en",
}