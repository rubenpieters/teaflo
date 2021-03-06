import { Ability } from "../../definitions/ability";
import * as Ab from "../../definitions/ability";
import * as A from "../../definitions/actionf";
import * as S from "../../definitions/status";
import { AIDirection } from "../../definitions/ai";
import { EnUnit } from "../../definitions/unit";

export const a2l1_en_ab1: Ability =
  A.combinedAbility([
    new A.AddStatus("Ability", "Target", new Ab.Static(new S.Weak(1)), Ab.allFriendly()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a2l1_en_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(15), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("left" as AIDirection), Ab.self()),
  ]);

export const a2l1_en: EnUnit = {
  hp: 24,
  maxHp: 24,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a2l1_en_ab1,
      spriteId: "ab4",
      name: "l1_en_ab1",
    },
    1: {
      ability: a2l1_en_ab2,
      spriteId: "ab3",
      name: "l1_en_ab2",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a2l1_en",
}