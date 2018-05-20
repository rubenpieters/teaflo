import * as Random from "random-seed"

export function newRng(seed: string): Random.RandomSeed {
  return Random.create(seed)
}

export function rngHandler(rng: Random.RandomSeed) {
  return {
    integerInRange: (n1: number, n2: number) => {
      return () => {
        return rng.intBetween(n1, n2);
      }
    }
  }
}