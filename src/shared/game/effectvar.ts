import { CreatureId, GameState } from "src/shared/game/state";

type Static<A> = {
  tag: "Static",
  v: A,
}

type Self = {
  tag: "Self",
}

type NumberInput<A> = {
  tag: "NumberInput",
  v: number,
  f: (n: number) => A,
}

const self: EffectVar<CreatureId> = { tag: "Self" };

type EffectVar<A>
  = Static<A>
  | Self
  | NumberInput<A>
  ;

function evToF<A>(ev: EffectVar<A>): (a: any) => A {
  switch (ev.tag) {
    case "Static": {
      return (_: {}) => {
        return ev.v;
      }
    }
    case "Self": {
      return (obj: { selfId: CreatureId }) => {
        // Self EffectVar should only be created
        // through self: EffectVar<CreatureId>
        return <any>obj.selfId;
      }
    }
    case "NumberInput": {
      return (obj: { inputs: any[] }) => {
        return ev.f(obj.inputs[ev.v]);
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
    case "NumberInput": {
      return `<Input${ev.v}>`;
    }
  }
}

function damage(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  piercing: EffectVar<boolean>,
) {
  return {
    effect: (obj: { inputs: any[], state: GameState, selfId: CreatureId } ) => {
      return {
        tag: "Damage",
        target: evToF(target)(obj),
        value: evToF(value)(obj),
        piercing: evToF(piercing)(obj),
      };
    },
    description: `deal ${showEv(value)} to ${showEv(target)}`,
  }
}