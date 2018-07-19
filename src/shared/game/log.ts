import { ActionTarget, Action } from "src/shared/game/action";
import { Target, TargetSpec, showTarget } from "src/shared/game/target";
import { showStatus } from "src/shared/game/status";
import { showCrew } from "./crew";
import { showEnemy } from "./enemy";
import { showItem } from "./item";

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

export function showAction<T extends TargetSpec | Target>(action: Action<T>): string {
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
      return "Damage " + action.value + " to " + showTarget(action.target);
    }
    case "Heal": {
      return "Heal " + action.value + " to " + showTarget(action.target);
    }
    case "GainHP": {
      return "Gain " + action.value + " HP "  + "(" + showTarget(action.target) + ")";
    }
    case "GainAP": {
      return "Gain " + action.value + " AP "  + "(" + showTarget(action.target) + ")";
    }
    case "DamageAP": {
      return "DamageAP " + action.value + " to " + showTarget(action.target);
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
      return "AddStatus " + showStatus(action.status) + " to " + showTarget(action.target);
    }
    case "Noop": {
      return "Noop";
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