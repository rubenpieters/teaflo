// https://github.com/gcanti/typelevel-ts/blob/7a5bc28e3d01e82beb26c1a84160cdfbe81ffe43/src/index.ts
export type Overwrite<A extends object, B extends object> =
  Pick<A, Exclude<keyof A, keyof B>> & B;

export type Omit<A extends object, K extends string | number | symbol> =
  Pick<A, Exclude<keyof A, K>>;


export type Equal<A, B> =
  A extends B
  ? ( B extends A ? true : false )
  : false
  ;

export type If<Cond extends boolean, Then, Else> =
  Cond extends true ? Then : Else;
