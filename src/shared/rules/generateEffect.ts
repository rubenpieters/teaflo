import { Rng, chooseSet } from "src/shared/handler/rng/randomSeedRng";
import { NodeEffect } from "src/shared/rules/effect";

type GenerateRow = {
  generate: (rng: Rng) => { effect: NodeEffect, cost: number }
}

const loseBasic: GenerateRow = {
  generate: rng => {
    const loseAmount: number = rng.integerInRange(1, 8)();
    const cost: number = loseAmount;
    const effect: NodeEffect = {
      tag: "LoseEffect",
      loss: {
        color: "Basic",
        type: "Both",
        amount: loseAmount,
      }
    }
    return { effect: effect, cost: cost };
  }
}

const destroyMod: GenerateRow = {
  generate: rng => {
    const pos: number = rng.integerInRange(1, 10)();
    const cost: number = 5;
    const effect: NodeEffect = { tag: "DestroyModEffect", position: pos };
    return { effect: effect, cost: cost }
  }
}

export const negEffects: GenerateRow[] = [
  loseBasic,
  destroyMod,
]

// genPoints is the amount of points we can spend to generate the effect
export function generateEffects(genPoints: number, rng: Rng, genRows: GenerateRow[]): NodeEffect[] {
  let leftoverPoints: number = genPoints;
  let effectAcc: NodeEffect[] = [];

  while (leftoverPoints > genPoints * 0.1) {
    const { effect, cost } = generateEffect(rng, genRows);
    if (leftoverPoints - cost >= 0) {
      leftoverPoints -= cost;
      effectAcc = effectAcc.concat([effect]);
    }
    // else leftoverPoints would go below 0 => don't use generated effect
  }
  return effectAcc;
}

function generateEffect(rng: Rng, genRows: GenerateRow[]): { effect: NodeEffect, cost: number } {
  const row: GenerateRow = chooseSet(rng, genRows);
  return row.generate(rng);
}