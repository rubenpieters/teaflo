export class DescSymbol {
  constructor(
    public readonly symbol: string,
    public readonly tag: "DescSymbol" = "DescSymbol",
  ) {}
}

export class DescSeparator {
  constructor(
    public readonly tag: "DescSeparator" = "DescSeparator",
  ) {}
}

export type DescToken
  = DescSymbol
  | DescSeparator
  ;