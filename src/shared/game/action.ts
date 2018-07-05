import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState, IdCrew } from "src/shared/game/state";
import { Enemy, runBattle } from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Target, findTarget, onTargets } from "src/shared/game/target";

export type Recruit = {
  tag: "Recruit",
  crew: Crew,
}

export type Battle = {
  tag: "Battle",
  enemy: Enemy,
}

export type Rest = {
  tag: "Rest",
}

export type Damage = {
  tag: "Damage",
  positions: number[],
  value: number,
}

export type GainHP<T> = {
  tag: "GainHP",
  target: T,
  value: number,
}

export type GainAP<T> = {
  tag: "GainAP",
  target: T,
  value: number,
}

export type BattleTurn = {
  tag: "BattleTurn",
  turn: number,
}

export type Action<T>
  = Recruit
  | Battle
  | Damage
  | BattleTurn
  | GainHP<T>
  | GainAP<T>

function fmap<A,B>(
  f: (a: A) => B,
  action: Action<A>,
): Action<B> {
  switch (action.tag) {
    case "Recruit": return action
    case "Battle": return action
    case "Damage": return action
    case "BattleTurn": return action
    case "GainHP": {
      return {...action, ...{ target: f(action.target)}};
    }
    case "GainAP": {
      return {...action, ...{ target: f(action.target)}};
    }
  }
}

export type ActionRest = Action<Target> | Rest;

export function doAction(
  action: ActionRest,
  state: GameState,
  log: ActionRest[],
  from: number,
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  let newState: GameState = state;
  let newLog: ActionRest[] = log;

  // crew interactions with effects
  for (const ally of state.crew.slice(from)) {
    for (const trigger of ally.triggers) {
      if (trigger.onTag === action.tag && trigger.type === "before") {
        const action = fmap(x => findTarget(x, ally.id), trigger.action);
        const afterTrigger = doAction(action, newState, newLog, from + 1, idGen);
        if (afterTrigger.newState === "invalid") {
          return { newState: "invalid", newLog };
        }
        newState = afterTrigger.newState;
        newLog = afterTrigger.newLog;
      }
    }
  }
  
  const afterEffectLog = newLog.concat([action])
  switch (action.tag) {
    case "Rest": {
      return { newState, newLog: afterEffectLog };
    }
    case "Damage": {
      let resultCrew: IdCrew[] = newState.crew;
      for (const position in action.positions) {
        const allyAtPos: IdCrew | undefined = newState.crew[position];
        if (allyAtPos === undefined) {
          return { newState: "invalid", newLog: afterEffectLog }
        }
        resultCrew = focus(resultCrew,
          over(x => x[position].hp, x => x - action.value)
        )
      }
      newState = focus(newState, set(x => x.crew, resultCrew));
      return { newState, newLog: afterEffectLog };
    }
    case "Battle": {
      const afterBattle = runBattle(newState, action.enemy, afterEffectLog);
      if (afterBattle.newState === "invalid") {
        return { newState: "invalid", newLog: afterBattle.newLog };
      }
      return { newState: afterBattle.newState, newLog: afterBattle.newLog };
    }
    case "Recruit": {
      const id = idGen.newId();
      const idCrew = {...action.crew, ...{ id }}
      newState = focus(newState,
        over(x => x.crew, x => [idCrew].concat(x))
      );
      return { newState, newLog: afterEffectLog };
    }
    case "BattleTurn": {
      return { newState, newLog: afterEffectLog };
    }
    case "GainHP": {
      const addHP = (c: IdCrew) => focus(c, over(x => x.hp, x => x + action.value));
      newState = onTargets(action.target, addHP, newState);
      return { newState, newLog: afterEffectLog };
    }
    case "GainAP": {
      const addAP = (c: IdCrew) => focus(c, over(x => x.ap, x => x + action.value));
      newState = onTargets(action.target, addAP, newState);
      return { newState, newLog: afterEffectLog };
    }
  }
}