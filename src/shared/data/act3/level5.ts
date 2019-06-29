import { Ability } from "../../definitions/ability";
import * as Ab from "../../definitions/ability";
import * as A from "../../definitions/actionf";
import * as S from "../../definitions/status";
import { AIDirection } from "../../definitions/ai";
import { EnUnit, FrUnit } from "../../definitions/unit";
import { TargetInput, EnemyInput, FriendlyInput } from "../../definitions/input";
import * as C from "../../definitions/condition";

/**
 * En Unit
 */
export const a3l5_en1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(4), Ab.highestThreat()),
  ]);

export const a3l5_en1_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(8), Ab.highestThreat()),
  ]);

export const a3l5_en1: EnUnit = {
  hp: 40,
  maxHp: 40,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a3l5_en1_ab1,
      spriteId: "ab3",
      name: "a3l5_en1_ab1",
    },
    1: {
      ability: a3l5_en1_ab2,
      spriteId: "ab3",
      name: "a3l5_en1_ab2",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a3l5_en1",
}