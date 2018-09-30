import { CreatureId, GameState } from "src/shared/game/state";
import { Status } from "./status";
import { Action } from "./action";
import { InputType } from "./input";
import { InputEntityEffect, EntityEffect } from "./ability";
import { cursorTo } from "readline";


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

type EffectVar<A>
  = EffectVarNoInput<A>
  | NumberInput<A>
  ;

type EffectVarNoInput<A>
  = Static<A>
  | Self
  | AllAlly
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
    case "AllAlly": {
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
  }
}

function evToInput<A>(ev: EffectVar<A>): InputType | undefined {
  switch (ev.tag) {
    case "Static": {
      return undefined;
    }
    case "Self": {
      return undefined;
    }
    case "NumberInput": {
      return {
        tag: "NumberInput",
      };
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

export function evAllAlly(
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
      }
    },
    description: f({ tag: "AllAlly" }).description,
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

//function f(n: number): CreatureId { return <any>undefined };
//const x = damageI({ tag: "NumberInput", v: 0, f: f }, { tag: "Static", v: 10 }, { tag: "Static", v: false });