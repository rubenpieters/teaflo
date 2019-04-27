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
  new A.Damage("Ability", "Target", new Ab.Static(5), new Ab.FromInput(0))
  ;

export const a1l2_fr_ab2: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(5), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(20), new Ab.FromInput(0)),
  ]);

export const a1l2_fr: FrUnit = {
  hp: 20,
  maxHp: 20,
  charges: 3,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l2_fr_ab1,
      inputs: [
        new FriendlyInput(),
      ],
      spriteId: "ab3",
      name: "a1l2_fr_ab1",
    },
    {
      ability: a1l2_fr_ab2,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab3",
      name: "a1l2_fr_ab2",
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
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.highestThreat()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l2_en_ab2: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(50), Ab.highestThreat()),
  ]);

export const a1l2_en: EnUnit = {
  hp: 20,
  maxHp: 20,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l2_en_ab1,
      spriteId: "ab3",
      name: "a1l2_en_ab1",
    },
    1: {
      ability: a1l2_en_ab1,
      spriteId: "ab3",
      name: "a1l2_en_ab1",
    },
    2: {
      ability: a1l2_en_ab2,
      spriteId: "ab3",
      name: "a1l2_en_ab2",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l2_en",
}