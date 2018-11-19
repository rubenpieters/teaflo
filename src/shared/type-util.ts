// https://github.com/gcanti/typelevel-ts/blob/7a5bc28e3d01e82beb26c1a84160cdfbe81ffe43/src/index.ts
export type Overwrite<A extends object, B extends object> =
  Pick<A, Exclude<keyof A, keyof B>> & B;