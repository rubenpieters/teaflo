import * as fc from "fast-check";
import { Action } from "../src/shared/definitions/action";
import * as A from "../src/shared/definitions/actionf";
import { Ability } from "../src/shared/definitions/ability";
import * as Ab from "../src/shared/definitions/ability";
import { TargetType } from "../src/shared/definitions/entityId";
import { FrUnit, EnUnit } from "../src/shared/definitions/unit";
import { FriendlyInput, EnemyInput, TargetInput } from "../src/shared/definitions/input";
import { mkGameStateWithUnit, showGamestate } from "../src/shared/game/state";
import { frUnitMap } from "../src/shared/data/frUnitMap";
import { enUnitMap } from "../src/shared/data/enUnitMap";
import { trySolutions } from "../test/util";
import { AIDirection } from "../src/shared/definitions/ai";

/**
 * Fr Unit
 */

function createFr1Ab1(
  cost: number,
  value: number,
): Ability {
  return A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(cost), Ab.self()),
    new A.RestoreCharge("Ability", "Target", new Ab.Static(value), new Ab.FromInput(0)),
  ]);
}

function createFrUnit1(
  ab1_cost: number,
  ab1_value: number,
): FrUnit {
  const ab1 = createFr1Ab1(ab1_cost, ab1_value);
  return  {
    hp: 15,
    maxHp: 15,
    charges: 5,
    maxCharges: 5,
    abilities: [
      {
        ability: ab1,
        inputs: [
          new FriendlyInput(),
        ],
        spriteId: "ab3",
        name: "ab",
      },
    ],
    essential: true,
    cardId: undefined as any,
  }
}

function createFr2Ab1(
  cost: number,
  value: number,
): Ability {
  return A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(cost), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(value), new Ab.FromInput(0)),
  ]);
}

function createFrUnit2(
  ab1_cost: number,
  ab1_value: number,
): FrUnit {
  return {
    hp: 21,
    maxHp: 21,
    charges: 0,
    maxCharges: 6,
    abilities: [
      {
        ability: createFr2Ab1(ab1_cost, ab1_value),
        inputs: [
          new EnemyInput(),
        ],
        spriteId: "ab1",
        name: "a1l5_fr2_ab1",
      },
    ],
    essential: true,
    cardId: undefined as any,
  }
}


function createFr3Ab1(
  cost: number,
  value: number,
): Ability {
  return A.combinedAbility([
    new A.UseCharge("Ability", "Target", new Ab.Static(cost), new Ab.Self()),
    new A.Damage("Ability", "Target", new Ab.Static(value), new Ab.FromInput(0)),
  ]);
}

function createFrUnit3(
  ab1_cost: number,
  ab1_value: number,
): FrUnit {
  return {
    hp: 15,
    maxHp: 15,
    charges: 0,
    maxCharges: 6,
    abilities: [
      {
        ability: createFr3Ab1(ab1_cost, ab1_value),
        inputs: [
          new EnemyInput(),
        ],
        spriteId: "ab1",
        name: "a1l5_fr3_ab1",
      },
    ],
    essential: true,
    cardId: undefined as any,
  }
}

/**
 * En Unit
 */

export const a1l5_en1_ab1: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l5_en1_ab2: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.MoveAI("Ability", "Target", new Ab.Static("right" as AIDirection), Ab.self()),
  ]);

export const a1l5_en1_ab3: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.MoveAI("Ability", "Target", new Ab.Static("down" as AIDirection), Ab.self()),
  ]);

export const a1l5_en1_ab4: Ability =
  A.combinedAbility([
    new A.RestoreCharge("Ability", "Target", new Ab.Static(1), Ab.self()),
    new A.MoveAI("Ability", "Target", new Ab.Static("left" as AIDirection), Ab.self()),
  ]);

function en1_ab5(
  val: number
): Ability {
  return A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(val), Ab.highestThreat()),
  ]);
}

function createEnUnit1(
  hp_val: number,
  dmg_val: number,
): EnUnit {
  return {
    hp: hp_val,
    maxHp: hp_val,
    charges: 0,
    maxCharges: 5,
    abilities: {
      0: {
        ability: a1l5_en1_ab1,
        spriteId: "ab3",
        name: "a1l5_en1_ab1",
      },
      1: {
        ability: a1l5_en1_ab2,
        spriteId: "ab3",
        name: "a1l5_en1_ab2",
      },
      2: {
        ability: a1l5_en1_ab3,
        spriteId: "ab3",
        name: "a1l5_en1_ab3",
      },
      5: {
        ability: a1l5_en1_ab4,
        spriteId: "ab3",
        name: "a1l5_en1_ab4",
      },
      4: {
        ability: en1_ab5(dmg_val),
        spriteId: "ab1",
        name: "a1l5_en1_ab5",
      },
    },
    essential: true,
    aiPosition: { x: 0, y: 0 },
    cardId: "a1l5_en1",
  }
}

function en2_ab1(
  val: number,
): Ability {
  return A.combinedAbility([
    new A.Damage("Ability", "Target", new Ab.Static(val), Ab.highestThreat()),
  ]);
}

function createEnUnit2(
  hp_val: number,
  dmg_val: number,
): EnUnit {
  return {
    hp: hp_val,
    maxHp: hp_val,
    charges: 5,
    maxCharges: 5,
    abilities: {
      0: {
        ability: en2_ab1(dmg_val),
        spriteId: "ab3",
        name: "en2_ab1",
      },
    },
    essential: true,
    aiPosition: { x: 0, y: 0 },
    cardId: undefined as any,
  }
}


const params = [
  [1,2],[1,3],[1,4],[2,3],[2,4],[3,4]
];

params.forEach(l => {
  console.log("-----------------------------------------------------");
  console.log(`PARAMS -- ${l}`)

  // console.log(JSON.stringify(createFr1Ab1(l[0], l[1])));

  const initState = mkGameStateWithUnit(
    [createFrUnit1(l[0], l[1]), createFrUnit2(1, 1), createFrUnit3(1, 1)],
    [createEnUnit1(1, 1), createEnUnit2(1, 1)],
  );

  trySolutions(initState, 10);
  console.log("-----------------------------------------------------");
});

