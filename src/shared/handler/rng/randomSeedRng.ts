import * as Random from "random-seed"

export type Rng = {
  integerInRange: (n1: number, n2: number) => () => number
};

export function chooseSet<A>(rng: Rng, array: A[]): A {
  const l: number = rng.integerInRange(0, array.length - 1)();
  return array[l];
}

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