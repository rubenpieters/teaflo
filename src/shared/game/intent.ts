import { UnitId, isGlobalId, isPositionId, TargetType, GlobalId, PositionId } from "./entityId";
import { GameState, filteredEn, filteredFr, FrStUnit } from "./state";
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

export class AllAlly {
  constructor(
    public readonly tag: "AllAlly" = "AllAlly",
  ) {}
}

export function mkAllAlly(): IntentVar<UnitId> {
  return new AllAlly;
}

export class HighestThreat {
  constructor(
    public readonly tag: "HighestThreat" = "HighestThreat",
  ) {}
}

export function mkHighestThreat(): IntentVar<UnitId> {
  return new HighestThreat;
}

export class AllExceptSelf {
  constructor(
    public readonly tag: "AllExceptSelf" = "AllExceptSelf",
  ) {}
}

export function mkAllExceptSelf(): IntentVar<UnitId> {
  return new AllExceptSelf;
}

export type IntentVar<A>
  = Static<A>
  | FromInput
  | Self
  | AllEnemy
  | AllAlly
  | HighestThreat
  | AllExceptSelf
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

export class SwapHPWithExcessI {
  constructor(
    public readonly target1: IntentVar<UnitId>,
    public readonly target2: IntentVar<UnitId>,
    public readonly tag: "SwapHPWithExcessI" = "SwapHPWithExcessI",
  ) {}
}

export type Intent
  = DamageI
  | HealI
  | UseChargeI
  | CombinedIntent
  | AddThreatI
  | AddTriggerI
  | SwapHPWithExcessI
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
        context,
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
        context,
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
        context,
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
        context,
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
        context,
        intent.target,
        tgt => {
          return new A.AddTrigger(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.trigger),
          );
        }
      );
    }
    case "SwapHPWithExcessI": {
      return evaluateTargets(
        state,
        context,
        intent.target1,
        tgt => {
          return new A.SwapHPWithExcess(
            evaluateIntentVar(state, context, tgt),
            evaluateIntentVar(state, context, intent.target2),
          );
        }
      );
    }
  }
}

function evaluateTargets(
  state: GameState,
  context: Context,
  target: IntentVar<UnitId>,
  create: (target: IntentVar<UnitId>) => Action,
) {
  if (target.tag === "AllEnemy") {
    const actions = filteredEn(state)
      .map(x => create(new Static(new GlobalId(x.id, "enemy"))));
    return new A.CombinedAction(actions);
  } else if (target.tag === "AllAlly") {
    const actions = filteredFr(state)
      .map(x => create(new Static(new GlobalId(x.id, "friendly"))));
    return new A.CombinedAction(actions);
  } else if (target.tag === "AllExceptSelf") {
    const self = context.self;
    if (self === undefined) {
      console.log("evaluateTargets: no self given");
      throw "evaluateTargets: no self given";
    }
    const actionsFr = filteredFr(state)
      .filter(x => x.id !== self.id)
      .map(x => create(new Static(new GlobalId(x.id, "friendly"))));
    const actionsEn = filteredEn(state)
      .filter(x => x.id !== self.id)
      .map(x => create(new Static(new GlobalId(x.id, "enemy"))));
    return new A.CombinedAction(actionsFr.concat(actionsEn));
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
    case "AllAlly": {
      console.log("AllAlly: Internal Intent Var");
      throw "AllAlly: Internal Intent Var";
    }
    case "HighestThreat": {
      const self = context.self;
      if (self === undefined) {
        console.log("evaluateIntentVar: no self given");
        throw "evaluateIntentVar: no self given";
      }
      if (self.type !== "enemy") {
        console.log("evaluateIntentVar: self not enemy");
        throw "evaluateIntentVar: self not enemy";
      }
      const threat = filteredFr(state)
        .reduce((prev, curr) => {
          if (prev === undefined) {
            return curr;
          }
          if (prev.threatMap[self.id] === undefined) {
            return curr;
          }
          if (curr.threatMap[self.id] === undefined) {
            return prev;
          }
          if (curr.threatMap[self.id] >= prev.threatMap[self.id]) {
            return curr;
          }
          return prev;
        }, <FrStUnit | undefined>undefined);
      if (threat === undefined) {
        return <A>(<any>new PositionId(0, "friendly"));
      } else {
        return <A>(<any>new GlobalId(threat.id, "friendly"));
      }
    }
    case "AllExceptSelf": {
      console.log("AllExceptSelf: Internal Intent Var");
      throw "AllExceptSelf: Internal Intent Var";
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
    case "AllAlly": {
      return `<All Ally>`;
    }
    case "HighestThreat": {
      return `<Threat>`;
    }
    case "AllExceptSelf": {
      return `<All Except Self>`;
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