import { ActionTarget, Spec } from "src/shared/game/action";
import { Target, TargetSpec, showTarget } from "src/shared/game/target";
import { showStatus, Status } from "src/shared/game/status";
import { showCondition } from "src/shared/game/trigger";

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
  crewStatus: StatusLog[],
  crewAction: ActionTarget[],
  queue1: ActionTarget[],
  enemyStatus: StatusLog[],
  enemyAction: ActionTarget[],
  queue2: ActionTarget[],
  deaths: ActionTarget[], // TODO: incorporate into queue2
};

export type StatusLog = {
  id: number,
  status: Status["tag"],
  actionLog: ActionTarget[],
}

function showActionLog(actionLog: ActionLog): string {
  return "** " + showAction(actionLog.action) + " **\n"
    + "crew status\n"
    + showStatusLog(actionLog.crewStatus) + "\n"
    + "crew action\n"
    + actionLog.crewAction.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "queue1\n"
    + actionLog.queue1.map(a => " - " + showAction(a)).join("\n") + "\n"
    + "enemy status\n"
    + showStatusLog(actionLog.enemyStatus) + "\n"
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

export function actionShort<T extends TargetSpec | Target>(action: Spec<T>): string {
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
      return "Damage";
    }
    case "ApDamage": {
      return "Damage";
    }
    case "Heal": {
      return "Heal";
    }
    case "GainHP": {
      return `Gain ${action.value} HP (${showTarget(action.target)})`;
    }
    case "GainAP": {
      return `Gain ${action.value} AP (${showTarget(action.target)})`;
    }
    case "DamageAP": {
      return `DamageAP ${action.value} to ${showTarget(action.target)}`;
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
      return `QueueStatus to ${showTarget(action.target)}`;
    }
    case "AddStatus": {
      return `AddStatus to ${showTarget(action.target)}`;
    }
    case "Noop": {
      return "Noop";
    }
    case "ConditionAction": {
      return "ConditionAction";
    }
    case "Swap": {
      return `Swap ${action.type} ${action.from} ${action.to}`;
    }
    case "CombinedAction": {
      return `Combined ${action.actions.length}`;
    }
    case "CombinedSpec": {
      return `CombinedSpec ${action.actions.length}`;
    }
    case "DeathSelf": {
      return "DeathSelf";
    }
    case "ArmorBash": {
      return "ArmorBash";
    }
    case "ClearStatus": {
      return `Clear ${action.status}`;
    }
    case "ClearAllStatus": {
      return `Clear all status`;
    }
  }
}

export function showAction<T extends TargetSpec | Target>(action: Spec<T>): string {
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
      return `Damage ${action.value} to ${showTarget(action.target)}`;
    }
    case "ApDamage": {
      return `Damage ${action.multiplier} * (AP) to ${showTarget(action.target)}`;
    }
    case "Heal": {
      return `Heal ${action.value} to ${showTarget(action.target)}`;
    }
    case "GainHP": {
      return `Gain ${action.value} HP (${showTarget(action.target)})`;
    }
    case "GainAP": {
      return `Gain ${action.value} AP (${showTarget(action.target)})`;
    }
    case "DamageAP": {
      return `DamageAP ${action.value} to ${showTarget(action.target)}`;
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
      return `QueueStatus ${showStatus(action.status)} to ${showTarget(action.target)}`;
    }
    case "AddStatus": {
      return `AddStatus ${showStatus(action.status)} to ${showTarget(action.target)}`;
    }
    case "Noop": {
      return "Noop";
    }
    case "ConditionAction": {
      return `if ${action.conditions.map(showCondition).join(" & ")} then ${showAction(action.trueAction)} else ${showAction(action.falseAction)}`;
    }
    case "Swap": {
      return `Swap ${action.type} ${action.from} ${action.to}`;
    }
    case "CombinedAction": {
      return `Combined ${action.actions.length}`;
    }
    case "CombinedSpec": {
      return `CombinedSpec ${action.actions.length}`;
    }
    case "DeathSelf": {
      return "DeathSelf";
    }
    case "ArmorBash": {
      return "ArmorBash";
    }
    case "ClearStatus": {
      return `Clear ${action.status}`;
    }
    case "ClearAllStatus": {
      return `Clear all status`;
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