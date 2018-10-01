import { CreatureId, GameState } from "src/shared/game/state";
import { Status } from "src/shared/game/status";
import { Action } from "src/shared/game/action";
import { InputType } from "src/shared/game/input";

export type Eff1<A> = {
  effect: (obj: Context) => { action: Action } & A,
  description: string,
}

export type EffI<A> = Eff1<A> & { inputs: InputType[] };
export type EffT<A> = Eff1<A> & { type: "before" | "instead" };

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

type Trigger = {
  tag: "Trigger",
}

type AllAlly = {
  tag: "AllAlly",
}

type AllEnemy = {
  tag: "AllEnemy",
}

type AllyPos = {
  tag: "AllyPos",
}

type EffectVar<A>
  = Static<A>
  | Self
  | AllAlly
  | AllEnemy
  | AllyPos
  | Input<A>
  | Trigger
  ;

export type Context = {
  state: GameState,
  selfId?: CreatureId,
  trigger?: Action,
  inputs?: any[],
}

export function addTarget<A>(
  n: number,
  f: (target: EffectVar<CreatureId>) => Eff1<A>
): EffI<A> {
  const effS = f({ tag: "Input", v: n, f: numberToTarget});
  return { ...effS, inputs: [{ tag: "NumberInput" }], };
}

export function addTargetI<A>(
  n: number,
  eff: (a: EffectVar<CreatureId>) => EffI<A>,
): EffI<A> {
  const effS = eff({ tag: "Input", v: n, f: numberToTarget});
  return { ...effS, inputs: effS.inputs.concat({ tag: "NumberInput" }), };
}

export function noTarget<A>(
  eff: Eff1<A>,
): EffI<A> { 
  return { ...eff, inputs: [], };
}

export function evTrigger<A>(
  eff: (a: EffectVar<Action>) => Eff1<A>,
  type: "before" | "instead",
): EffT<A> {
  const effS = eff({ tag: "Trigger"});
  return {
    effect: effS.effect,
    description: effS.description,
    type,
  }
}

export function extra<A extends {}>(
  eff: Eff1<{}>,
  extra: A,
): Eff1<A> {
  return {
    effect: (obj) => {
      const action: Action = eff.effect(obj).action
      return { ...<any>extra, action };
    },
    description: eff.description,
  }
}

// TODO: description for condition
export function evCondition<A, C>(
  ev: EffectVar<C>,
  condition: (a: C) => boolean,
  fT: Eff1<A>,
  fF: Eff1<A>,
): Eff1<A> {
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
  f: (target: EffectVar<CreatureId>) => Eff1<{}>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const actions = obj.state.crew.map((ally, position) =>
        f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: position })).effect(obj).action,
      );
      const action: Action = {
        tag: "CombinedAction",
        actions,
      };
      return { action };
    },
    description: f({ tag: "AllAlly" }).description,
  }
}

export function evEnemies(
  f: (target: EffectVar<CreatureId>) => Eff1<{}>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const actions = obj.state.enemies.map((enemy, position) =>
        f(evStatic(<CreatureId>{ tag: "PositionId", type: "enemy", id: position })).effect(obj).action,
      );
      const action: Action = {
        tag: "CombinedAction",
        actions,
      };
      return { action };
    },
    description: f({ tag: "AllEnemy" }).description,
  }
}


export function evAllyPositions(
  f: (target: EffectVar<CreatureId>) => Eff1<{}>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const indices = [...Array(obj.state.crew.length).keys()]
      const actions = indices.map(x => f(evStatic(<CreatureId>{ tag: "PositionId", type: "ally", id: x })).effect(obj).action);
      const action: Action = {
        tag: "CombinedAction",
        actions,
      };
      return { action };
    },
    description: f({ tag: "AllyPos" }).description,
  }
}

export function evAnd(
  ...effs: Eff1<{}>[]
): Eff1<{}> {
  return {
    effect: (obj) => {
      const actions = effs.reduce((prev, curr) => prev.concat(curr.effect(obj).action), <Action[]>[]);
      const action: Action = {
        tag: "CombinedAction",
        actions,
      };
      return { action };
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
          console.log("Context does not have selfId");
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
          console.log("Context does not have inputs");
          throw "Context does not have inputs";
        } else {
          return ev.f(context.inputs[ev.v]);
        }
      }
      case "Trigger": {
        if (context.trigger === undefined) {
          console.log("Context does not have trigger");
          throw "Context does not have trigger";
        } else {
          return <A>(<any>context.trigger);
        }
      }
      default: {
        console.log("Internal Effect Var");
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
    case "AllAlly": {
      return "<Allies>";
    }
    case "AllEnemy": {
      return "<Enemies>";
    }
    case "AllyPos": {
      return "<Ally Positions>";
    }
    case "Input": {
      return `<Input${ev.v}>`;
    }
    case "Trigger": {
      return `<Trigger>`;
    }
  }
}

export function damage(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  piercing: EffectVar<boolean>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const action: Action = {
        tag: "Damage",
        target: evaluate(target)(obj),
        value: evaluate(value)(obj),
        piercing: evaluate(piercing)(obj),
      };
      return { action };
    },
    description: `deal ${showEv(value)} to ${showEv(target)}`,
  }
}

export function queueStatus(
  target: EffectVar<CreatureId>,
  status: EffectVar<Status>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const action: Action = {
        tag: "QueueStatus",
        target: evaluate(target)(obj),
        status: evaluate(status)(obj),
      };
      return { action };
    },
    description: `queue ${showEv(status)} to ${showEv(target)}`,
  }
}
export function chargeUse(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const action: Action = {
        tag: "ChargeUse",
        target: evaluate(target)(obj),
        value: evaluate(value)(obj),
      };
      return { action };
    },
    description: `${showEv(target)} use ${showEv(value)} charge`,
  }
}

export function heal(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const action: Action = {
        tag: "Heal",
        target: evaluate(target)(obj),
        value: evaluate(value)(obj),
      };
      return { action };
    },
    description: `heal ${showEv(value)} to ${showEv(target)}`,
  }
}

export function addThreat(
  target: EffectVar<CreatureId>,
  value: EffectVar<number>,
  enemyId: EffectVar<CreatureId>,
): Eff1<{}> {
  return {
    effect: (obj) => {
      const action: Action = {
        tag: "AddThreat",
        target: evaluate(target)(obj),
        value: evaluate(value)(obj),
        enemyId: evaluate(enemyId)(obj),
      };
      return { action };
    },
    description: `add ${showEv(value)} threat for ${showEv(target)} on ${showEv(enemyId)}`,
  }
}

export function noop(
): Eff1<{}> {
  return {
    effect: (_obj) => {
      const action: Action = { tag: "Noop" };
      return { action };
    },
    description: "Noop",
  }
};