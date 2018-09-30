import { CreatureId, GameState } from "src/shared/game/state";
import { Status } from "./status";
import { Action } from "./action";
import { InputType } from "./input";
import { InputEntityEffect, EntityEffect, TriggerEntityEffect } from "./ability";
import { cursorTo } from "readline";

type Eff1 = {
  effect: (obj: Context) => Action,
  description: string,
}

type Static<A> = {
  tag: "Static",
  v: A,
}

export function evStatic<A>(a: A): EffectVar<A> {
  return {
    tag: "Static", v: a
  }
}

type Self = {
  tag: "Self",
}

export const evSelf: EffectVar<CreatureId> = { tag: "Self" };

type Input<A> = {
  tag: "Input",
  v: number,
  f: (n: number) => A,
}

function numberToTarget(input: number): CreatureId {
  if (input >= 0) {
    return { tag: "PositionId", type: "enemy", id: input }
  } else {
    return { tag: "PositionId", type: "ally", id: (-input) - 1 }
  }
}

export function evInput(n: number): EffectVar<CreatureId> {
  return {
    tag: "Input",
    v: n,
    f: numberToTarget
  }
}

type AllAlly = {
  tag: "AllAlly",
}

type AllyPos = {
  tag: "AllyPos",
}

type EffectVar<A>
  = Static<A>
  | Self
  | AllAlly
  | AllyPos
  | Input<A>
  ;

type Context = {
  state: GameState,
  selfId?: CreatureId,
  trigger?: Action,
  inputs?: any[],
}

export function target(
  n: number,
  f: (target: EffectVar<CreatureId>) => Eff1
): Eff1 & { inputs: InputType[] } {
  const effS = f({ tag: "Input", v: n, f: numberToTarget});
  return {
    effect: effS.effect,
    description: effS.description,
    inputs: [{ tag: "NumberInput" }],
  }
}

export function evCondition<A>(
  ev: EffectVar<A>,
  condition: (a: A) => boolean,
  fT: Eff1,
  fF: Eff1,
): Eff1 {
  return {
    effect: (obj) => {
      if (condition(evaluate(ev)(obj))) {
        return fT.effect(obj); 
      } else {
        return fF.effect(obj);
      }
    },
    description: `${showEv(ev)} ? ${fT.description} : ${fF.description}`,
  }
}

export function evAllies(
  f: (target: EffectVar<CreatureId>) => Eff1
): Eff1 {
  return {
    effect: (obj) => {
      const actions = obj.state.crew.map((ally, position) =>
        f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: position })).effect(obj),
      );
      return {
        tag: "CombinedAction",
        actions,
      };
    },
    description: f({ tag: "AllAlly" }).description,
  }
}

export function evAllyPositions(
  f: (target: EffectVar<CreatureId>) => Eff1
): Eff1 {
  return {
    effect: (obj) => {
      const indices = [...Array(obj.state.crew.length).keys()]
      const actions = indices.map(x => f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: x })).effect(obj));
      return {
        tag: "CombinedAction",
        actions,
      };
    },
    description: f({ tag: "AllyPos" }).description,
  }
}

export function evAnd(
  ...effs: Eff1[]
): Eff1 {
  return {
    effect: (obj) => {
      return {
        tag: "CombinedAction",
        actions: effs.reduce((prev, curr) => prev.concat(curr.effect(obj)), <Action[]>[]),
      };
    },
    description: effs.reduce((prev, curr) => `${prev} and ${curr.description}`, ""),
  }
}

function evaluate<A>(
  ev: EffectVar<A>
): (context: Context) => A {
  return (context: Context) => {
    switch (ev.tag) {
      case "Self": {
        if (context.selfId === undefined) {
          throw "Context does not have selfId";
        } else {
          return <A>(<any>context.selfId);
        }
      }
      case "Static": {
        return ev.v;
      }
      case "Input": {
        if (context.inputs === undefined) {
          throw "Context does not have inputs";
        } else {
          return ev.f(context.inputs[ev.v]);
        }
      }
      default: {
        throw "Internal Effect Var";
      }
    }
  }
}

function showEv<A>(ev: EffectVar<A>): string {
  switch (ev.tag) {
    case "Static": {
      return `${JSON.stringify(ev.v)}`;
    }
    case "Self": {
      return `<Self>`;
    }
    case "Input": {
      return `<Input${ev.v}>`;
    }
    case "AllAlly": {
      return "<Allies>";
    }
    case "AllyPos": {
      return "<Ally Positions>";
    }
  }
}

export function damage(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  piercing: EffectVar<boolean>,
): Eff1 {
  return {
    effect: (obj: Context) => {
      return <Action>{
        tag: "Damage",
        target: evaluate(target)(obj),
        value: evaluate(value)(obj),
        piercing: evaluate(piercing)(obj),
      };
    },
    description: `deal ${showEv(value)} to ${showEv(target)}`,
  }
}

/*
export type InputEntityEffectNoI = {
  effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId }) => Action,
  description: string,
}

type Static<A> = {
  tag: "Static",
  v: A,
}

export function evStatic<A>(a: A): EffectVar<A> {
  return {
    tag: "Static", v: a
  }
}

type Self = {
  tag: "Self",
}

export const evSelf: EffectVar<CreatureId> = { tag: "Self" };

type NumberInput<A> = {
  tag: "NumberInput",
  v: number,
  f: (n: number) => A,
}

function numberToTarget(input: number): CreatureId {
  if (input >= 0) {
    return { tag: "PositionId", type: "enemy", id: input }
  } else {
    return { tag: "PositionId", type: "ally", id: (-input) - 1 }
  }
}

export function evInput(n: number): EffectVar<CreatureId> {
  return {
    tag: "NumberInput",
    v: n,
    f: numberToTarget
  }
}

type AllAlly = {
  tag: "AllAlly",
}

type AllyPos = {
  tag: "AllyPos",
}

type EffectVar<A>
  = EffectVarNoInput<A>
  | NumberInput<A>
  ;

type EffectVarNoInput<A>
  = Static<A>
  | Self
  | AllAlly
  | AllyPos
  ;

type EvToFInput<A, EV extends EffectVar<A>>
  = EV extends { tag: "Static" } ? {}
  : EV extends { tag: "Self" } ? { selfId: CreatureId }
  : EV extends { tag: "NumberInput" } ? { inputs: any[] }
  : never
  ;

type EvToFOutput<A, EV extends EffectVar<A>>
= EV extends { tag: "Static" } ? A
: EV extends { tag: "Self" } ? CreatureId
: EV extends { tag: "NumberInput" } ? A
: never
;

type UnionToIntersection<U> = 
  (U extends any ? (k: U)=>void : never) extends ((k: infer I)=>void) ? I : never

function evToF<A, EV extends EffectVar<A>>(
  ev: EV
): (i: UnionToIntersection<EvToFInput<A, EV>>) => EvToFOutput<A, EV>
 {
  switch (ev.tag) {
    case "Static": {
      return <any>((_: {}) => {
        const v: A = (<any>ev).v;
        return v;
      })
    }
    case "Self": {
      return <any>((obj: { selfId: CreatureId }) => {
        // Self EffectVar should only be created
        // through self: EffectVar<CreatureId>
        return obj.selfId;
      })
    }
    case "NumberInput": {
      const f: (n: number) => A = (<any>ev).f;
      const v: number = (<any>ev).v;
      return <any>((obj: { inputs: any[] }) => {
        return f(obj.inputs[v]);
      })
    }
    default: {
      throw "internal effect var";
    }
  }
  throw "evToF: impossible case";
}

function showEv<A>(ev: EffectVar<A>): string {
  switch (ev.tag) {
    case "Static": {
      return `${JSON.stringify(ev.v)}`;
    }
    case "Self": {
      return `<Self>`;
    }
    case "NumberInput": {
      return `<Input${ev.v}>`;
    }
    case "AllAlly": {
      return "<Allies>";
    }
    case "AllyPos": {
      return "<Ally Positions>";
    }
  }
}

export function withI(
  eff: InputEntityEffectNoI,
  ...inputs: InputType[]
): InputEntityEffect {
  return {
    effect: eff.effect,
    description: eff.description,
    inputs,
  }
}

export function target(
  n: number,
  eff: (a: EffectVar<CreatureId>) => InputEntityEffectNoI,
): InputEntityEffect {
  const effS = eff({ tag: "NumberInput", v: n, f: numberToTarget});
  return {
    effect: effS.effect,
    description: effS.description,
    inputs: [{ tag: "NumberInput" }],
  }
}

export function addTarget(
  n: number,
  eff: (a: EffectVar<CreatureId>) => InputEntityEffect,
): InputEntityEffect {
  const effS = eff({ tag: "NumberInput", v: n, f: numberToTarget});
  return {
    effect: effS.effect,
    description: effS.description,
    inputs: effS.inputs.concat({ tag: "NumberInput" }),
  }
}

export function evAllies(
  f: (target: EffectVar<CreatureId>) => InputEntityEffectNoI
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      const actions = obj.state.crew.map((ally, position) =>
        f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: position })).effect(obj),
      );
      return {
        tag: "CombinedAction",
        actions,
      };
    },
    description: f({ tag: "AllAlly" }).description,
  }
}

export function evAllyPositions(
  f: (target: EffectVar<CreatureId>) => InputEntityEffectNoI
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      const indices = [...Array(obj.state.crew.length).keys()]
      const actions = indices.map(x => f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: x })).effect(obj));
      return {
        tag: "CombinedAction",
        actions,
      };
    },
    description: f({ tag: "AllyPos" }).description,
  }
}

export function evAnd(
  ...effs: InputEntityEffectNoI[]
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return {
        tag: "CombinedAction",
        actions: effs.reduce((prev, curr) => prev.concat(curr.effect(obj)), <Action[]>[]),
      };
    },
    description: effs.reduce((prev, curr) => `${prev} and ${curr.description}`, ""),
  }
}

export function damageI(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  piercing: EffectVar<boolean>,
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "Damage",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
        piercing: evToF(piercing)(obj),
      };
    },
    description: `deal ${showEv(value)} to ${showEv(target)}`,
  }
}

export function damage(
  target: EffectVarNoInput<CreatureId>,
  value: EffectVarNoInput<number>,
  piercing: EffectVarNoInput<boolean>,
): EntityEffect {
  return {
    effect: (obj: { state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "Damage",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
        piercing: evToF(piercing)(obj),
      };
    },
    description: `deal ${showEv(value)} to ${showEv(target)}`,
  }
}


export function queueStatusI(
  target: EffectVar<CreatureId>,
  status: EffectVar<Status>,
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "QueueStatus",
        target: evToF(target)(obj),
        status: evToF(status)(obj),
      };
    },
    description: `queue ${showEv(status)} to ${showEv(target)}`,
  }
}

export function queueStatus(
  target: EffectVarNoInput<CreatureId>,
  status: EffectVarNoInput<Status>,
): EntityEffect {
  return {
    effect: (obj: { state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "QueueStatus",
        target: evToF(target)(obj),
        status: evToF(status)(obj),
      };
    },
    description: `queue ${showEv(status)} to ${showEv(target)}`,
  }
}

export function chargeUseI(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "ChargeUse",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
      };
    },
    description: `${showEv(target)} use ${showEv(value)} charge`,
  }
}

export function chargeUse(
  target: EffectVarNoInput<CreatureId>,
  value: EffectVarNoInput<number>,
): EntityEffect {
  return {
    effect: (obj: { state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "ChargeUse",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
      };
    },
    description: `${showEv(target)} use ${showEv(value)} charge`,
  }
}

export function healI(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "Heal",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
      };
    },
    description: `heal ${showEv(value)} to ${showEv(target)}`,
  }
}

export function heal(
  target: EffectVarNoInput<CreatureId>,
  value: EffectVarNoInput<number>,
): EntityEffect {
  return {
    effect: (obj: { state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "Heal",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
      };
    },
    description: `heal ${showEv(value)} to ${showEv(target)}`,
  }
}

export function addThreatI(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  enemyId: EffectVar<number>,
): InputEntityEffectNoI {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "AddThreat",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
        enemyId: evToF(enemyId)(obj),
      };
    },
    description: `add ${showEv(value)} threat for ${showEv(target)} on ${showEv(enemyId)}`,
  }
}

export function addThreat(
  target: EffectVarNoInput<CreatureId>,
  value: EffectVarNoInput<number>,
  enemyId: EffectVarNoInput<number>,
): EntityEffect {
  return {
    effect: (obj: { state: GameState, selfId: CreatureId } ) => {
      return <Action>{
        tag: "AddThreat",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
        enemyId: evToF(enemyId)(obj),
      };
    },
    description: `add ${showEv(value)} threat for ${showEv(target)} on ${showEv(enemyId)}`,
  }

  */