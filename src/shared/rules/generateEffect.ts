import { Rng, chooseSet } from "src/shared/handler/rng/randomSeedRng";
import { NodeEffect } from "src/shared/rules/effect";

type GenerateRow = {
  generate: (rng: Rng) => { effect: NodeEffect, cost: number }
}

const loseBasic: GenerateRow = {
  generate: rng => {
    const amount: number = rng.integerInRange(1, 8)();
    const cost: number = amount;
    const effect: NodeEffect = {
      tag: "LoseEffect",
      loss: {
        color: "Basic",
        type: "Both",
        amount: amount,
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

const gainBasic: GenerateRow = {
  generate: rng => {
    const amount: number = rng.integerInRange(1, 8)();
    const cost: number = amount;
    const effect: NodeEffect = {
      tag: "LoseEffect",
      loss: {
        color: "Basic",
        type: "Both",
        amount: amount,
      }
    }
    return { effect: effect, cost: cost };
  }
}

const addBuffer: GenerateRow = {
  generate: rng => {
    const bufferValue: number = rng.integerInRange(1, 8)();
    const maxCharges: number = rng.integerInRange(1, 3)();
    const cost: number = bufferValue + (maxCharges * 2);
    const effect: NodeEffect = {
      tag: "AddModifier",
      modifier: {
        charges: maxCharges,
        chargePerUse: 1,
        maxCharges: maxCharges,
        modifierEffect: {
          tag: "Buffer",
          value: bufferValue,
        },
      }
    }
    return { effect: effect, cost: cost };
  }
}

const convertStackTempToTotal: GenerateRow = {
  generate: rng => {
    const amount: number = rng.integerInRange(1, 8)();
    const cost: number = amount;
    const effect: NodeEffect = {
      tag: "ConvertEffect",
      converts:
        {
          tag: "ConvertUnit",
          from: {
            color: "Stack",
            type: "Temp",
          },
          to: {
            color: "Stack",
            type: "Total",
          },
          amount: amount,
        }
    }
    return { effect: effect, cost: cost };
  }
}

const addPersister: GenerateRow = {
  generate: rng => {
    const persisterValue: number = rng.integerInRange(1, 8)();
    const maxCharges: number = rng.integerInRange(1, 3)();
    const cost: number = persisterValue + (maxCharges * 2);
    const effect: NodeEffect = {
      tag: "AddModifier",
      modifier: {
        charges: maxCharges,
        chargePerUse: 1,
        maxCharges: maxCharges,
        modifierEffect: {
          tag: "Persister",
          cap: persisterValue,
        },
      }
    }
    return { effect: effect, cost: cost };
  }
}

const addIncreaseGain: GenerateRow = {
  generate: rng => {
    const increaseAmount: number = rng.integerInRange(1, 8)();
    const maxCharges: number = rng.integerInRange(1, 3)();
    const cost: number = increaseAmount * maxCharges;
    const effect: NodeEffect = {
      tag: "AddModifier",
      modifier: {
        charges: maxCharges,
        chargePerUse: 1,
        maxCharges: maxCharges,
        modifierEffect: {
          tag: "IncreaseGain",
          value: increaseAmount,
        },
      }
    }
    return { effect: effect, cost: cost };
  }
}

const gainCharge: GenerateRow = {
  generate: rng => {
    const cost: number = 5;
    const effect: NodeEffect = {
      tag: "GainChargeEffect",
      value: 1,
    }
    return { effect: effect, cost: cost }
  }
}

const addDuplicater: GenerateRow = {
  generate: rng => {
    const maxCharges: number = rng.integerInRange(1, 3)();
    const cost: number = maxCharges * 5;
    const effect: NodeEffect = {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "DuplicateAddMod",
        },
      }
    }
    return { effect: effect, cost: cost };
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