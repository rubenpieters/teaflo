import { ActionRest } from "src/shared/game/action";

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
  action: ActionRest,
  loggedEffects: ActionRest[],
}

function showActionLog(actionLog: ActionLog): string {
  return showAction(actionLog.action) + "\n" + actionLog.loggedEffects.map(a => " - " + showAction(a)).join("\n");
}

function showAction(action: ActionRest): string {
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
    case "GainHP": {
      return "Gain " + action.value + " HP (" + JSON.stringify(action.target) + ")";
    }
    case "GainAP": {
      return "Gain " + action.value + " AP (" + JSON.stringify(action.target) + ")";
    }
    case "GainGold": {
      return "GainGold " + action.gain;
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