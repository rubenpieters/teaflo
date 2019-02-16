import { Intent, IntentVar } from "../../shared/game/intent";
import { Trigger, triggerToFragmentValue } from "../../shared/game/trigger";

export function intentDescription(
  intent: Intent
): string[] {
  switch (intent.tag) {
    case "AddThreatI": {
      return intentVarDescription(intent.value, x => intentVarNumber(x, "positive"))
        .concat(intentVarDescription(intent.atEnemy, intentVarTarget))
        .concat(["expl_th.png"])
        .concat(intentVarDescription(intent.toFriendly, intentVarTarget))
        ;
    }
    case "AddTriggerI": {
      return intentVarDescription(intent.trigger, triggerDescription)
        .concat(intentVarDescription(intent.target, intentVarTarget))
        ;
    }
    case "CombinedIntent": {
      const desc: string[] = intent.intents.reduce((acc, x) => {
        return acc.concat(intentDescription(x));
      }, <string[]>[]);
      return desc;
    }
    case "DamageI": {
      return intentVarDescription(intent.value, x => intentVarNumber(x, "negative"))
        .concat(["expl_hp.png"])
        .concat(intentVarDescription(intent.target, intentVarTarget))
        ;
    }
    case "HealI": {
      return [];
    }
    case "SwapHPWithExcessI": {
      return [];
    }
    case "UseChargeI": {
      return intentVarDescription(intent.value, x => intentVarNumber(x, "negative"))
        .concat(["expl_ch.png"])
        .concat(intentVarDescription(intent.target, intentVarTarget))
        ;
    }
  }
}

export function triggerDescription(
  trigger: Trigger,
): string[] {
  switch (trigger.tag) {
    case "Weak": {
      return []
    }
    case "AllyWeakSelfArmor": {
      return []
    }
    case "Explode": {
      return []
    }
    case "Fragile": {
      return []
    }
    case "Grow": {
      return []
    }
    case "Strong": {
      return []
    }
    case "StrongLowHP": {
      return []
    }
    case "ThreatOnAllyDamage": {
      return []
    }
    case "Armor": {
      return ["expl_plus.png"]
        .concat(numberDescription(Math.round((trigger.fragments / triggerToFragmentValue(trigger)) - 0.5)))
        .concat(["expl_armor.png"])
        ;
    }
  }
}

function intentVarTarget(
  x: any
) {
  // TODO: implement
  return [];
}

function intentVarNumber(
  x: number,
  sign: "positive" | "negative",
) {
  if (sign === "positive") {
    return ["expl_plus.png"].concat(numberDescription(x));
  } else {
    return ["expl_minus.png"].concat(numberDescription(x));
  }
}

export function intentVarDescription<A>(
  intentVar: IntentVar<A>,
  f: (a: A) => string[]
) {
  switch (intentVar.tag) {
    case "Static": {
      return f(intentVar.a);
    }
    case "AllAlly": {
      return ["expl_all_friendly.png"];
    }
    case "AllEnemy": {
      return ["expl_all_enemy.png"];
    }
    case "AllExceptSelf": {
      return [];
    }
    case "AllyExceptSelf": {
      return [];
    }
    case "FromInput": {
      return ["expl_target.png"];
    }
    case "HighestThreat": {
      return ["expl_target_status.png"];
    }
    case "Self": {
      return ["expl_self.png"];
    }
  }
}

export function numberDescription(
  x: number,
): string[] {
  return _numberDescription(x, []);
}

function _numberDescription(
  x: number,
  acc: string[],
): string[] {
  const digit = x % 10;
  const next = Math.round((x / 10) - 0.5);
  if (next >= 1) {
    return _numberDescription(next, acc).concat([`expl_${digit}.png`]);
  } else {
    return acc.concat([`expl_${digit}.png`]);
  }
}