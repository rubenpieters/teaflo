import { TargetId } from "../definitions/entityId";
import { UserInput } from "../definitions/input";

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