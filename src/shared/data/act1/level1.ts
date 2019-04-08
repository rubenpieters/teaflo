import { Ability } from "../../game/ability";
import * as Ab from "../../game/ability";
import * as A from "../../game/action";
import * as S from "../../game/status";
import { AIDirection } from "../../game/ai";
import { EnUnit } from "../../game/unit";

export const l1_en_ab1: Ability =
  A.combinedAbility([
    new A.AddStatus("Ability", "Target", new Ab.Static(new S.Weak(1)), Ab.allFriendly()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const l1_en_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(15), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("left" as AIDirection), Ab.self()),
  ]);

export const l1_en: EnUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: l1_en_ab1,
      spriteId: "ab4",
      name: "l1_en_ab1",
    },
    1: {
      ability: l1_en_ab2,
      spriteId: "ab3",
      name: "l1_en_ab2",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "l1_en",
}