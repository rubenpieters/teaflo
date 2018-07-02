import { Action, Rest } from "src/shared/game/action";

export type SolutionLog = {
  actionLog: ActionLog[],
}

export const emptySolutionLog: SolutionLog = {
  actionLog: [],
}

export function showSolutionLog(solutionLog: SolutionLog): string {
  return solutionLog.actionLog
    .map(showActionLog)
    .join("\n");
}

export type ActionLog = {
  action: Action | Rest,
}

function showActionLog(actionLog: ActionLog): string {
  switch (actionLog.action.tag) {
    case "Battle": {
      return "-- Battle"
    }
    case "Recruit": {
      return "-- Recruit"
    }
    case "Rest": {
      return "-- Rest"
    }
  }
}