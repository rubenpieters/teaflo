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
export const a3l4_en1_2_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(3), Ab.lowestHp()),
  ]);

export const a3l4_en1_2: EnUnit = {
  hp: 32,
  maxHp: 32,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a3l4_en1_2_ab1,
      spriteId: "ab3",
      name: "a3l4_en1_ab1_2",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a3l4_en1_2",
}