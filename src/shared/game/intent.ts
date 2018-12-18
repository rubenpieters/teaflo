import { UnitId, isGlobalId, isPositionId, TargetType, GlobalId } from "./entityId";
import { GameState, filteredEn } from "./state";
import { Action } from "./action";
import * as A from "./action";
import { Trigger } from "./trigger";

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

export class AllEnemy {
  constructor(
    public readonly tag: "AllEnemy" = "AllEnemy",
  ) {}
}

export function mkAllEnemy(): IntentVar<UnitId> {
  return new AllEnemy;
}

export type IntentVar<A>
  = Static<A>
  | FromInput
  | Self
  | AllEnemy
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

export class AddTriggerI {
  constructor(
    public readonly target: IntentVar<UnitId>,
    public readonly trigger: IntentVar<Trigger>,
    public readonly tag: "AddTriggerI" = "AddTriggerI",
  ) {}
}

export type Intent
  = DamageI
  | HealI
  | UseChargeI
  | CombinedIntent
  | AddThreatI
  | AddTriggerI
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
  input?: any[],
  self?: UnitId,
}

export function intentToAction(
  state: GameState,
  context: Context,
  intent: Intent,
): Action {
  switch (intent.tag) {
    case "DamageI": {
      return evaluateTargets(
        state,
        intent.target,
        tgt => {
          return new A.Damage(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.value),
          );
        }
      );
    }
    case "HealI": {
      return evaluateTargets(
        state,
        intent.target,
        tgt => {
          return new A.Heal(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.value),
          );
        }
      );
    }
    case "UseChargeI": {
      return evaluateTargets(
        state,
        intent.target,
        tgt => {
          return new A.UseCharge(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.value),
          );
        }
      );
    }
    case "CombinedIntent": {
      const actions = intent.intents.map(x => intentToAction(state, context, x));
      return new A.CombinedAction(actions);
    }
    case "AddThreatI": {
      return evaluateTargets(
        state,
        intent.atEnemy,
        tgt => {
          return new A.AddThreat(
            evaluateIntentVar(state, context, intent.toFriendly),
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.value),
          );
        }
      );
    }
    case "AddTriggerI": {
      return evaluateTargets(
        state,
        intent.target,
        tgt => {
          return new A.AddTrigger(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.trigger),
          );
        }
      );
    }
  }
}

function evaluateTargets(
  state: GameState,
  target: IntentVar<UnitId>,
  create: (target: IntentVar<UnitId>) => Action,
) {
  if (target.tag === "AllEnemy") {
    const actions = filteredEn(state)
      .map(x => create(new Static(new GlobalId(x.id, "enemy"))));
    return new A.CombinedAction(actions)
  } else {
    return create(target);
  }
}

function evaluateIntentVar<A>(
  state: GameState,
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
    case "AllEnemy": {
      console.log("AllEnemy: Internal Intent Var");
      throw "AllEnemy: Internal Intent Var";
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
    case "AllEnemy": {
      return `<All Enemy>`;
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