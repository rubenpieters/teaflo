import * as Random from "random-seed"

const seededRng: Random.RandomSeed = Random.create("very-random-seed")

function integerInRange(n1: number, n2: number) {
  return function() {
    return seededRng.intBetween(n1, n2);
  }
}

export const rng = {
  integerInRange: integerInRange
}
