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
export const a1l10_fr1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(30), Ab.self()),
    new A.Damage("Ability", "Target", new Ab.Static(30), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(30), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l10_fr1: FrUnit = {
  hp: 1,
  maxHp: 1,
  charges: 1,
  maxCharges: 40,
  abilities: [
    {
      ability: a1l10_fr1_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "a1l10_fr1_ab1",
    },
  ],
  essential: true,
  cardId: "a1l10_fr1",
}

/**
 * En Unit
 */
export const a1l10_en1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(3), Ab.highestThreat()),
  ]);

export const a1l10_en1: EnUnit = {
  hp: 60,
  maxHp: 60,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l10_en1_ab1,
      spriteId: "ab3",
      name: "a1l10_en1_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l10_en1",
}