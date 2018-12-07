import { UnitId, isGlobalId, isPositionId, TargetType } from "./entityId";
import { GameState } from "./state";
import { Action, mkDamage, mkHeal, mkUseCharge, mkCombinedAction } from "./action";

type Static<A> = {
  tag: "Static",
  a: A,
};

export function mkStatic<A>(
  a: A,
): Static<A> {
  return {
    tag: "Static",
    a,
  }
}

type FromInput = {
  tag: "FromInput",
  input: number,
}

export function mkFromInput(
  input: number,
): FromInput {
  return {
    tag: "FromInput",
    input,
  }
}

export type IntentVar<A>
  = Static<A>
  | FromInput
  ;

type DamageI = {
  tag: "DamageI",
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
};

export function mkDamageI(
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
): DamageI {
  return {
    tag: "DamageI",
    target,
    value,
  }
}

type HealI = {
  tag: "HealI",
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
};

export function mkHealI(
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
): HealI {
  return {
    tag: "HealI",
    target,
    value,
  }
}

type UseChargeI = {
  tag: "UseChargeI",
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
}

export function mkUseChargeI(
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
): UseChargeI {
  return {
    tag: "UseChargeI",
    target,
    value,
  }
}

type CombinedIntent = {
  tag: "CombinedIntent",
  intents: Intent[],
}

export function mkCombinedIntent(
  ...intents: Intent[]
): CombinedIntent {
  return {
    tag: "CombinedIntent",
    intents,
  }
}

export type Intent
  = DamageI
  | HealI
  | UseChargeI
  | CombinedIntent
  ;

export type Context = {
  state: GameState,
  input?: any[],
}

export function intentToAction(
  context: Context,
  intent: Intent,
): Action {
  switch (intent.tag) {
    case "DamageI": {
      return mkDamage(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "HealI": {
      return mkHeal(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "UseChargeI": {
      return mkUseCharge(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "CombinedIntent": {
      const actions = intent.intents.map(x => intentToAction(context, x));
      return mkCombinedAction(actions);
    }
  }
}

function evaluateIntentVar<A>(
  context: Context,
  intentVar: IntentVar<A>,
): A {
  switch (intentVar.tag) {
    case "Static": {
      return intentVar.a;
    }
    case "FromInput": {
      if (context.input === undefined) {
        console.log("evaluateIntentVar: no input given");
        throw "evaluateIntentVar: no input given";
      }
      return context.input[intentVar.input];
    }
  }
}

export function intentVarText<A>(
  intentVar: IntentVar<A>,
): string {
  switch (intentVar.tag) {
    case "Static": {
      const a = intentVar.a;
      if (isGlobalId<TargetType>(a)) {
        return `<GID ${abbreviateTargetType(a.type)} ${a.id}>`;
      } else if (isPositionId<TargetType>(a)) {
        return `<POS ${abbreviateTargetType(a.type)} ${a.id}>`;
      }
      return JSON.stringify(intentVar.a);
    }
    case "FromInput": {
      return `<Input${intentVar.input}>`;
    }
  }
}

function abbreviateTargetType(
  type: TargetType,
) {
  switch (type) {
    case "enemy": return "EN";
    case "friendly": return "FR";
  }
}