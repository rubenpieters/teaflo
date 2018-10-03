import { Action } from "src/shared/game/action";
import { showTarget } from "src/shared/game/target";
import { showStatus, Status } from "src/shared/game/status";
import { showId } from "./state";

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
  action: Action,
  startTurn: Action[],
  loseFragment: Action[],
  crewAction: Action[],
  queue1: Action[],
  enemyAction: Action[],
  queue2: Action[],
  deaths: Action[], // TODO: incorporate into queue2
};

export type StatusLog = {
  id: number,
  status: Status["tag"],
  actionLog: Action[],
}

function showActionLog(actionLog: ActionLog): string {
  return "** " + showAction(actionLog.action) + " **\n"
    + "start turn\n"
    + actionLog.startTurn.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "crew action\n"
    + actionLog.crewAction.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "queue1\n"
    + actionLog.queue1.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "enemy action\n"
    + actionLog.enemyAction.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "queue2\n"
    + actionLog.queue2.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "deaths\n"
    + actionLog.deaths.map(a => " - " + showAction(a)).join("\n")
    ;
}

function showStatusLog(statusLog: StatusLog[]): string {
  let result: string = "";
  for (const { id, status, actionLog } of statusLog) {
    result += `  + ${status} + ${id}\n`;
    result += actionLog.map(a => "   - " + showAction(a)).join("\n") + "\n";
  }
  return result;
}

function showAction(action: Action): string {
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
    case "AddInstance": {
      return "AddInstance";
    }
    case "Rest": {
      return "Rest";
    }
    case "Damage": {
      return `Damage ${action.value} to ${showId(action.target)}`;
    }
    case "Heal": {
      return `Heal ${action.value} to ${showId(action.target)}`;
    }
    case "GainHP": {
      return `Gain ${action.value} HP (${showId(action.target)})`;
    }
    case "GainAP": {
      return `Gain ${action.value} AP (${showId(action.target)})`;
    }
    case "GainGold": {
      return `GainGold ${action.gain}`;
    }
    case "PayGold": {
      return `PayGold ${action.pay}`;
    }
    case "BattleTurn": {
      return "BattleTurn";
    }
    case "Death": {
      return `Death ${action.type} ${action.id}`;
    }
    case "QueueStatus": {
      return `QueueStatus ${showStatus(action.status)} to ${showId(action.target)}`;
    }
    case "AddStatus": {
      return `AddStatus ${showStatus(action.status)} to ${showId(action.target)}`;
    }
    case "Noop": {
      return "Noop";
    }
    case "Swap": {
      return `Swap ${action.type} ${action.from} ${action.to}`;
    }
    case "CombinedAction": {
      return `Combined ${action.actions.length}`;
    }
    case "ClearStatus": {
      return `Clear ${action.status}`;
    }
    case "ChargeUse": {
      return `ChargeUse ${action.value} ${showId(action.target)}`;
    }
    case "AddThreat": {
      return `AddThreat ${action.value} ${showId(action.target)}`;
    }
    case "StartTurn": {
      return `StartTurn`;
    }
    case "LoseFragment": {
      return `LoseFragment ${action.type} ${showId(action.target)}`;
    }
    case "Invalid": {
      return `Invalid`;
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