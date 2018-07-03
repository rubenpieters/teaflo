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
  action: (Action | Rest),
  loggedEffects: (Action | Rest)[],
}

function showActionLog(actionLog: ActionLog): string {
  return showAction(actionLog.action) + "\n" + actionLog.loggedEffects.map(a => " - " + showAction(a)).join("\n");
}

function showAction(action: Action | Rest): string {
  switch (action.tag) {
    case "Battle": {
      return "Battle";
    }
    case "Recruit": {
      return "Recruit";
    }
    case "Rest": {
      return "Rest";
    }
    case "Damage": {
      return "Damage";
    }
    case "BattleTurn": {
      return "BattleTurn - " + action.turn;
    }
  }
}

type EnemyDamage = {
  tag: "EnemyDamage",
}

type AllyDamage = {
  tag: "AllyDamage",
}

export type LoggedEffect
  = EnemyDamage
  | AllyDamage