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
export const a3l1_en1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(3), Ab.allFriendly()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a3l1_en1_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(12), Ab.highestThreat()),
  ]);

export const a3l1_en1: EnUnit = {
  hp: 40,
  maxHp: 40,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a3l1_en1_ab1,
      spriteId: "ab3",
      name: "a3l1_en2_ab1",
    },
    1: {
      ability: a3l1_en1_ab2,
      spriteId: "ab3",
      name: "a3l1_en2_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a3l1_en1",
}