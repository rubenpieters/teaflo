import { UnitId, isGlobalId, isPositionId, UnitType, GlobalId, PositionId, EntityId, toGlobalId } from "./entityId";
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

export class AllyExceptSelf {
  constructor(
    public readonly tag: "AllyExceptSelf" = "AllyExceptSelf",
  ) {}
}

export function mkAllyExceptSelf(): IntentVar<UnitId> {
  return new AllyExceptSelf;
}

export type IntentVar<A>
  = Static<A>
  | FromInput
  | Self
  | AllEnemy
  | AllAlly
  | HighestThreat
  | AllExceptSelf
  | AllyExceptSelf
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
): Action {
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
  } else if (target.tag === "AllyExceptSelf") {
    const self = context.self;
    if (self === undefined) {
      console.log("evaluateTargets: no self given");
      throw "evaluateTargets: no self given";
    }
    const actionsFr = filteredFr(state)
      .filter(x => x.id !== self.id)
      .map(x => create(new Static(new GlobalId(x.id, "friendly"))));
    return new A.CombinedAction(actionsFr);
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
      return context.self as any;
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
      return getHighestThreat(state, self) as any;
    }
    case "AllExceptSelf": {
      console.log("AllExceptSelf: Internal Intent Var");
      throw "AllExceptSelf: Internal Intent Var";
    }
    case "AllyExceptSelf": {
      console.log("AllyExceptSelf: Internal Intent Var");
      throw "AllyExceptSelf: Internal Intent Var";
    }
  }
}

export function getHighestThreat(
  state: GameState,
  self: EntityId<"friendly" | "enemy">,
): EntityId<"friendly" | "enemy"> {
  const globalId = toGlobalId(state, self);
  const threat = filteredFr(state)
    .reduce((prev, curr) => {
      if (prev === undefined) {
        return curr;
      }
      if (prev.threatMap[globalId.id] === undefined) {
        return curr;
      }
      if (curr.threatMap[globalId.id] === undefined) {
        return prev;
      }
      if (curr.threatMap[globalId.id] >= prev.threatMap[globalId.id]) {
        return curr;
      }
      return prev;
    }, <FrStUnit | undefined>undefined);
  if (threat === undefined) {
    return new PositionId(0, "friendly");
  } else {
    return new GlobalId(threat.id, "friendly");
  }
}

export function intentVarText<A>(
  intentVar: IntentVar<A>,
): string {
  switch (intentVar.tag) {
    case "Static": {
      const a = intentVar.a;
      if (isGlobalId<UnitType>(a)) {
        return `<GID ${abbreviateTargetType(a.type)} ${a.id}>`;
      } else if (isPositionId<UnitType>(a)) {
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
    case "AllyExceptSelf": {
      return `<Allies Except Self>`;
    }
  }
}

function abbreviateTargetType(
  type: UnitType,
) {
  switch (type) {
    case "enemy": return "EN";
    case "friendly": return "FR";
  }
}