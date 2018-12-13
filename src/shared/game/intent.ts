import { UnitId, isGlobalId, isPositionId, TargetType } from "./entityId";
import { GameState } from "./state";
import { Action } from "./action";
import * as A from "./action";

export class Static<A> {
  constructor(
    public readonly a: A,
    public readonly tag: "Static" = "Static",
  ) {}
}

export class FromInput {
  constructor(
    public readonly input: number,
    public readonly tag: "FromInput" = "FromInput",
  ) {}
}

export class Self {
  constructor(
    public readonly tag: "Self" = "Self",
  ) {}
}

export function mkSelf(): IntentVar<UnitId> {
  return new Self;
}

export type IntentVar<A>
  = Static<A>
  | FromInput
  | Self
  ;

export class DamageI {
  constructor(
    public readonly target: IntentVar<UnitId>,
    public readonly value: IntentVar<number>,
    public readonly tag: "DamageI" = "DamageI",
  ) {}
}

export class HealI {
  constructor(
    public readonly target: IntentVar<UnitId>,
    public readonly value: IntentVar<number>,
    public readonly tag: "HealI" = "HealI",
  ) {}
}

export class UseChargeI {
  constructor(
    public readonly target: IntentVar<UnitId>,
    public readonly value: IntentVar<number>,
    public readonly tag: "UseChargeI" = "UseChargeI",
  ) {}
}

export class CombinedIntent {
  constructor(
    public readonly intents: Intent[],
    public readonly tag: "CombinedIntent" = "CombinedIntent",
  ) {}
}

export class AddThreatI {
  constructor(
    public readonly toFriendly: IntentVar<UnitId>,
    public readonly atEnemy: IntentVar<UnitId>,
    public readonly value: IntentVar<number>,
    public readonly tag: "AddThreatI" = "AddThreatI",
  ) {}
}

export type Intent
  = DamageI
  | HealI
  | UseChargeI
  | CombinedIntent
  | AddThreatI
  ;

export function thDamage(
  target: IntentVar<UnitId>,
  value: IntentVar<number>,
) {
  return new CombinedIntent([
    new DamageI(target, value),
    new AddThreatI(mkSelf(), target, value),
  ]);
}

export type Context = {
  state: GameState,
  input?: any[],
  self?: UnitId,
}

export function intentToAction(
  context: Context,
  intent: Intent,
): Action {
  switch (intent.tag) {
    case "DamageI": {
      return new A.Damage(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "HealI": {
      return new A.Heal(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "UseChargeI": {
      return new A.UseCharge(
        evaluateIntentVar(context, intent.target),
        evaluateIntentVar(context, intent.value),
      );
    }
    case "CombinedIntent": {
      const actions = intent.intents.map(x => intentToAction(context, x));
      return new A.CombinedAction(actions);
    }
    case "AddThreatI": {
      return new A.AddThreat(
        evaluateIntentVar(context, intent.toFriendly),
        evaluateIntentVar(context, intent.atEnemy),
        evaluateIntentVar(context, intent.value),
      );
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
    case "Self": {
      if (context.self === undefined) {
        console.log("evaluateIntentVar: no self given");
        throw "evaluateIntentVar: no self given";
      }
      return <A>(<any>context.self);
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
    case "Self": {
      return `<Self>`;
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