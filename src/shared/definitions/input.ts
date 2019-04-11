export class TargetInput {
  public readonly tag: "TargetInput" = "TargetInput";

  constructor() {}
}

export class EnemyInput {
  public readonly tag: "EnemyInput" = "EnemyInput";

  constructor() {}
}

export class FriendlyInput {
  public readonly tag: "FriendlyInput" = "FriendlyInput";

  constructor() {}
}

export class StatusInput {
  public readonly tag: "StatusInput" = "StatusInput";

  constructor() {}
}

export class UnitInput {
  public readonly tag: "UnitInput" = "UnitInput";

  constructor() {}
}

export type UserInput
  = TargetInput
  | EnemyInput
  | FriendlyInput
  | StatusInput
  | UnitInput
  ;