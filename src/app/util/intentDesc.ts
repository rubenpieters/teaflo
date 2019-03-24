import { Intent, IntentVar } from "../../shared/game/intent";
import { Trigger, triggerToFragmentValue, triggerValue } from "../../shared/game/trigger";
import { Action } from "../../shared/game/action";
import { RouteDirection } from "src/shared/game/ai";

export class DescSymbol {
  constructor(
    public readonly sym: string,
    public readonly tag: "DescSymbol" = "DescSymbol",
  ) {}
}

export class DescSeparator {
  constructor(
    public readonly tag: "DescSeparator" = "DescSeparator",
  ) {}
}

export type DescToken
  = DescSymbol
  | DescSeparator

function singleton(
  str: string,
): DescToken[] {
  return [new DescSymbol(str)];
}

export function actionDescription(
  action: Action,
): DescToken[] {
  switch (action.tag) {
    case "AddThreat": {
      return singleton("expl_plus.png")
        .concat(numberDescription(action.value))
        .concat(new DescSymbol("expl_th.png"))
        ;
    }
    case "AddTrigger": {
      return triggerDescription(action.trigger)
        ;
    }
    case "CombinedAction": {
      throw "should not happen";
    }
    case "Damage": {
      return singleton("expl_minus.png")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("expl_hp.png"))
      ;
    }
    case "Heal": {
      return singleton("expl_plus.png")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("expl_hp.png"))
      ;
    }
    case "SwapHPWithExcess": {
      return [];
    }
    case "UseCharge": {
      return singleton("expl_minus.png")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("expl_ch.png"))
      ;
    }
    default: {
      return [];
    }
  }
}

export function intentDescription(
  intent: Intent,
): DescToken[] {
  switch (intent.tag) {
    case "AddThreatI": {
      return intentVarDescription(intent.value, x => intentVarNumber(x, "positive"))
        .concat(intentVarDescription(intent.atEnemy, intentVarTarget))
        .concat(new DescSymbol("expl_th.png"))
        .concat(intentVarDescription(intent.toFriendly, intentVarTarget))
        ;
    }
    case "AddTriggerI": {
      return intentVarDescription(intent.trigger, triggerDescription)
        .concat(intentVarDescription(intent.target, intentVarTarget))
        ;
    }
    case "CombinedIntent": {
      const desc: DescToken[] = intent.intents.reduce((acc, x) => {
        if (acc.length === 0) {
          return acc.concat(intentDescription(x));
        } else {
          return acc.concat(new DescSeparator()).concat(intentDescription(x));
        }
      }, <DescToken[]>[]);
      return desc;
    }
    case "DamageI": {
      return intentVarDescription(intent.value, x => intentVarNumber(x, "negative"))
        .concat(new DescSymbol("expl_hp.png"))
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
        .concat(new DescSymbol("expl_ch.png"))
        .concat(intentVarDescription(intent.target, intentVarTarget))
        ;
    }
  }
}

export function triggerDescription(
  trigger: Trigger,
): DescToken[] {
  switch (trigger.tag) {
    case "Weak": {
      return singleton("expl_plus.png")
        .concat(numberDescription(triggerValue(trigger)))
        .concat(new DescSymbol("icon_weak.png"))
        ;
    }
    case "AllyWeakSelfArmor": {
      return []
    }
    case "Explode": {
      return []
    }
    case "Fragile": {
      return singleton("expl_plus.png")
        .concat(numberDescription(triggerValue(trigger)))
        .concat(new DescSymbol("icon_fragile.png"))
        ;
    }
    case "Grow": {
      return []
    }
    case "Strong": {
      return singleton("expl_plus.png")
        .concat(numberDescription(triggerValue(trigger)))
        .concat(new DescSymbol("icon_strong.png"))
        ;
    }
    case "StrongLowHP": {
      return []
    }
    case "ThreatOnAllyDamage": {
      return []
    }
    case "Armor": {
      return singleton("expl_plus.png")
        .concat(numberDescription(triggerValue(trigger)))
        .concat(new DescSymbol("icon_armor.png"))
        ;
    }
  }
}


export function triggerTagDescription(
  tag: Trigger["tag"],
): DescToken[] {
  switch (tag) {
    case "Weak": {
      return singleton("icon_weak.png");
    }
    case "AllyWeakSelfArmor": {
      return []
    }
    case "Explode": {
      return []
    }
    case "Fragile": {
      return singleton("icon_fragile.png");
    }
    case "Grow": {
      return []
    }
    case "Strong": {
      return singleton("icon_strong.png");
    }
    case "StrongLowHP": {
      return []
    }
    case "ThreatOnAllyDamage": {
      return []
    }
    case "Armor": {
      return singleton("icon_armor.png");
    }
  }
}

function intentVarTarget(
  x: any
): DescToken[] {
  // TODO: implement
  return [];
}

function intentVarNumber(
  x: number,
  sign: "positive" | "negative",
): DescToken[] {
  if (sign === "positive") {
    return singleton("expl_plus.png").concat(numberDescription(x));
  } else {
    return singleton("expl_minus.png").concat(numberDescription(x));
  }
}

export function intentVarDescription<A>(
  intentVar: IntentVar<A>,
  f: (a: A) => DescToken[]
) {
  switch (intentVar.tag) {
    case "Static": {
      return f(intentVar.a);
    }
    case "AllAlly": {
      return [new DescSymbol("expl_all_friendly.png")];
    }
    case "AllEnemy": {
      return [new DescSymbol("expl_all_enemy.png")];
    }
    case "AllExceptSelf": {
      return [];
    }
    case "AllyExceptSelf": {
      return [];
    }
    case "FromInput": {
      return [new DescSymbol("expl_target.png")];
    }
    case "HighestThreat": {
      return [new DescSymbol("expl_target_status.png")];
    }
    case "Self": {
      return [new DescSymbol("expl_self.png")];
    }
  }
}

export function numberDescription(
  x: number,
): DescToken[] {
  return _numberDescription(x, []);
}

function _numberDescription(
  x: number,
  acc: DescToken[],
): DescToken[] {
  const digit = x % 10;
  const next = Math.round((x / 10) - 0.5);
  if (next >= 1) {
    return _numberDescription(next, acc).concat([new DescSymbol(`expl_${digit}.png`)]);
  } else {
    return acc.concat([new DescSymbol(`expl_${digit}.png`)]);
  }
}

export function routeDirectionDescription(
  routeDirection: RouteDirection,
) {
  switch (routeDirection) {
    case "down": return "icon_ai_down.png";
    case "left": return "icon_ai_left.png";
    case "right": return "icon_ai_right.png";
    case "up": return "icon_ai_up.png";
    case "self": return "icon_self.png";
  }
}