import { ActionTarget } from "src/shared/game/action";

export type SolutionLog = {
  actionLog: ActionLog[],
};

export const emptySolutionLog: SolutionLog = {
  actionLog: [],
};

export function showSolutionLog(solutionLog: SolutionLog): string {
  return solutionLog.actionLog
    .map(showActionLog)
    .join("\n");
}

export type ActionLog = {
  action: ActionTarget,
  loggedEffects: ActionTarget[],
};

function showActionLog(actionLog: ActionLog): string {
  return showAction(actionLog.action) + "\n" + actionLog.loggedEffects.map(a => " - " + showAction(a)).join("\n");
}

function showAction(action: ActionTarget): string {
  switch (action.tag) {
    case "AddEnemy": {
      return "AddEnemy";
    }
    case "AddCrew": {
      return "AddCrew";
    }
    case "AddItem": {
      return "AddItem";
    }
    case "Rest": {
      return "Rest";
    }
    case "Damage": {
      return "Damage " + action.value + " to " + JSON.stringify(action.target);
    }
    case "GainHP": {
      return "Gain " + action.value + " HP "  + "(" + JSON.stringify(action.target) + ")";
    }
    case "GainAP": {
      return "Gain " + action.value + " AP "  + "(" + JSON.stringify(action.target) + ")";
    }
    case "GainGold": {
      return "GainGold " + action.gain;
    }
    case "PayGold": {
      return "PayGold " + action.pay;
    }
    case "BattleTurn": {
      return "BattleTurn";
    }
    case "Death": {
      return "Death " + action.type + " " + action.id;
    }
    case "AddStatus": {
      return "AddStatus " + JSON.stringify(action.status) + " to " + JSON.stringify(action.target);
    }
  }
}

type EnemyDamage = {
  tag: "EnemyDamage",
};

type AllyDamage = {
  tag: "AllyDamage",
};

export type LoggedEffect
  = EnemyDamage
  | AllyDamage
  ;