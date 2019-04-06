export class TargetInput {
  public readonly tag: "TargetInput" = "TargetInput";
}

export class EnemyInput {
  public readonly tag: "EnemyInput" = "EnemyInput";

}

export class FriendlyInput {
  public readonly tag: "FriendlyInput" = "FriendlyInput";

}

export type UserInput
  = TargetInput
  | EnemyInput
  | FriendlyInput
  ;