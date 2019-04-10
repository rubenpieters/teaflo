import { TargetId } from "./entityId";

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


export function matchUserInput(
  userInput: UserInput,
  globalId: TargetId,
): boolean {
  switch (userInput.tag) {
    case "TargetInput": {
      return true;
    }
    case "UnitInput": {
      return globalId.type === "friendly" || globalId.type === "enemy";
    }
    case "StatusInput": {
      return globalId.type === "status";
    }
    case "EnemyInput": {
      return globalId.type === "enemy";
    }
    case "FriendlyInput": {
      return globalId.type === "friendly";
    }
  }
}