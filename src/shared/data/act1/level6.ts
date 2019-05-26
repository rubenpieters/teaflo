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
export const a1l6_fr1_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(2), new Ab.FromInput(0)),
  ]);

export const a1l6_fr1_ab2: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(3), new Ab.FromInput(0)),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
  ]);

export const a1l6_fr1: FrUnit = {
  hp: 15,
  maxHp: 15,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: a1l6_fr1_ab1,
      inputs: [
        new FriendlyInput(),
      ],
      spriteId: "ab3",
      name: "a1l6_fr1_ab1",
    },
    {
      ability: a1l6_fr1_ab2,
      inputs: [
        new FriendlyInput(),
      ],
      spriteId: "ab3",
      name: "a1l6_fr1_ab2",
    },
  ],
  essential: true,
  cardId: "a1l6_fr1",
}

export const a1l6_fr2_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(2), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(18), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(18), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l6_fr2: FrUnit = {
  hp: 51,
  maxHp: 51,
  charges: 0,
  maxCharges: 6,
  abilities: [
    {
      ability: a1l6_fr2_ab1,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab1",
      name: "a1l6_fr2_ab1",
    },
  ],
  essential: true,
  cardId: "a1l6_fr2",
}

export const a1l6_fr3_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
    new A.AddThreat("Ability", "Target", new Ab.Static(10), Ab.self(), new Ab.FromInput(0)),
  ]);

export const a1l6_fr3: FrUnit = {
  hp: 21,
  maxHp: 21,
  charges: 0,
  maxCharges: 6,
  abilities: [
    {
      ability: a1l6_fr3_ab1,
      inputs: [
        new EnemyInput(),
      ],
      spriteId: "ab1",
      name: "a1l6_fr3_ab1",
    },
  ],
  essential: true,
  cardId: "a1l6_fr2",
}

/**
 * En Unit
 */

export const a1l6_en1_ab1: Ability =
  A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(10), Ab.highestThreat()),
  ]);

export const a1l6_en1: EnUnit = {
  hp: 64,
  maxHp: 64,
  charges: 5,
  maxCharges: 5,
  abilities: {
    0: {
      ability: a1l6_en1_ab1,
      spriteId: "ab3",
      name: "a1l6_en2_ab1",
    },
  },
  essential: true,
  aiPosition: { x: 0, y: 0 },
  cardId: "a1l6_en1",
}