import { ActionTarget, Spec } from "src/shared/game/action";
import { Target, TargetSpec, showTarget } from "src/shared/game/target";
import { showStatus } from "src/shared/game/status";
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
  loggedEffects: ActionTarget[],
};

function showActionLog(actionLog: ActionLog): string {
  return showAction(actionLog.action) + "\n" + actionLog.loggedEffects.map(a => " - " + showAction(a)).join("\n");
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