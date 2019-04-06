import { FrUnit } from "src/shared/game/unit";
import { TargetInput } from "src/shared/game/input";
import { Ability } from "src/shared/game/ability";
import * as Ab from "src/shared/game/ability";
import * as A from "src/shared/game/action";

/**
 * Trinity Dmg Unit
 */
export const trinity_dmg_ab1: Ability =
  A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(1), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(10), new Ab.FromInput(0)),
  ]);

export const trinity_dmg: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      ability: trinity_dmg_ab1,
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "trinity_dmg_ab1",
    },
  ],
  essential: true,
}

/**
 * Trinity Tank Unit
 */
export const trinity_tnk_ab1: Ability =
A.combinedAbility([
  new A.UseCharge("Ability", "Target", new Ab.Static(1), new Ab.Self()),
  new A.AddThreat("Ability", "Target", new Ab.Static(13), Ab.self(), Ab.allEnemy()),
]);

export const trinity_tnk: FrUnit = {
hp: 11,
maxHp: 11,
charges: 5,
maxCharges: 5,
abilities: [
  {
    ability: trinity_tnk_ab1,
    inputs: [
      new TargetInput(),
    ],
    spriteId: "ab1",
    name: "trinity_tnk_ab1",
  },
],
essential: true,
}

/**
 * Trinity Support Unit
 */
export const trinity_sup_ab1: Ability =
A.combinedAbility([
  new A.UseCharge("Ability", "Target", new Ab.Static(1), new Ab.Self()),
  // add armor
]);

export const trinity_sup: FrUnit = {
hp: 5,
maxHp: 5,
charges: 5,
maxCharges: 5,
abilities: [
  {
    ability: trinity_dmg_ab1,
    inputs: [
      new TargetInput(),
    ],
    spriteId: "ab2",
    name: "trinity_sup_ab1",
  },
],
essential: true,
}