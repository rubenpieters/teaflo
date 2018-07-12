import { focus, over, set } from "src/shared/iassign-util";
import { Crew, damage } from "src/shared/game/crew";
import { GameState, IdCrew } from "src/shared/game/state";
import { Enemy, runBattle } from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Target, findTarget, onTargets, indexOfId } from "src/shared/game/target";
import { Item } from "src/shared/game/item";

export type Recruit = {
  tag: "Recruit",
  crew: Crew,
};

export type Battle = {
  tag: "Battle",
  enemy: Enemy,
};

export type Rest = {
  tag: "Rest",
};

export type Damage = {
  tag: "Damage",
  positions: number[],
  value: number,
};

export type GainHP<T> = {
  tag: "GainHP",
  target: T,
  value: number,
  type: "permanent" | "temporary",
};

export type GainAP<T> = {
  tag: "GainAP",
  target: T,
  value: number,
  type: "permanent" | "temporary",
};

export type BattleTurn = {
  tag: "BattleTurn",
  turn: number,
};

export type StartBattle = {
  tag: "StartBattle",
};

export type EndBattle = {
  tag: "EndBattle",
};

export type GainGold = {
  tag: "GainGold",
  gain: number,
};

export type PayGold = {
  tag: "PayGold",
  pay: number,
};

export type Death = {
  tag: "Death",
  targetId: number,
};

export type AddItem = {
  tag: "AddItem",
  item: Item,
};

export type ClearTemp = {
  tag: "ClearTemp",
};

export type Action<T>
  = Recruit
  | Battle
  | Damage
  | BattleTurn
  | GainHP<T>
  | GainAP<T>
  | GainGold
  | PayGold
  | Death
  | AddItem
  | StartBattle
  | EndBattle
  | Rest
  ;

export function fmap<A, B>(
  f: (a: A) => B,
  action: Action<A>,
): Action<B> {
  switch (action.tag) {
    case "Recruit": return action;
    case "Battle": return action;
    case "Damage": return action;
    case "BattleTurn": return action;
    case "StartBattle": return action;
    case "EndBattle": return action;
    case "GainHP": return {...action, ...{ target: f(action.target)}};
    case "GainAP": return {...action, ...{ target: f(action.target)}};
    case "GainGold": return action;
    case "PayGold": return action;
    case "Death": return action;
    case "AddItem": return action;
    case "Rest": return action;
  }
}

export type ActionRest = Action<Target> | Rest;

export function doAction(
  action: ActionRest,
  state: GameState,
  log: ActionRest[],
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  return doActionAt(action, state, log, { id: 0, type: "item" }, idGen);
}

export function doActionAt(
  action: ActionRest,
  state: GameState,
  log: ActionRest[],
  from: { id: number, type: "item" | "crew" },
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionRest[] } {
  let newState: GameState = state;
  let newLog: ActionRest[] = log;

  // item interactions with effects
  if (from.type === "item") {
    for (const item of state.items.slice(from.id)) {
      for (const trigger of item.triggers) {
        if (trigger.onTag === action.tag && trigger.type === "before") {
          // TODO: targeting items not supported
          const action = fmap(x => findTarget(x, newState, (<any>"trying to target item")), trigger.action);
          const afterTrigger = doActionAt(action, newState, newLog, { id: from.id + 1, type: "item" }, idGen);
          if (afterTrigger.newState === "invalid") {
            return { newState: "invalid", newLog };
          }
          newState = afterTrigger.newState;
          newLog = afterTrigger.newLog;
        }
      }
    }
  }

  const fromCrew = from.type === "crew" ? from.id : 0;
  // crew interactions with effects
  for (const ally of state.crew.slice(fromCrew)) {
    for (const trigger of ally.triggers) {
      if (trigger.onTag === action.tag && trigger.type === "before") {
        const action = fmap(x => findTarget(x, newState, ally.id), trigger.action);
        const afterTrigger = doActionAt(action, newState, newLog, { id: from.id + 1, type: "crew" }, idGen);
        if (afterTrigger.newState === "invalid") {
          return { newState: "invalid", newLog };
        }
        newState = afterTrigger.newState;
        newLog = afterTrigger.newLog;
      }
    }
  }

  const afterEffectLog = newLog.concat([action]);
  switch (action.tag) {
    case "Rest": {
      return { newState, newLog: afterEffectLog };
    }
    case "Damage": {
      let resultCrew: IdCrew[] = newState.crew;
      for (const position in action.positions) {
        const allyAtPos: IdCrew | undefined = newState.crew[position];
        if (allyAtPos === undefined) {
          return { newState: "invalid", newLog: afterEffectLog };
        }
        resultCrew = focus(resultCrew,
          over(x => x[position], x => damage(x, action.value))
        );
      }
      newState = focus(newState, set(x => x.crew, resultCrew));
      return { newState, newLog: afterEffectLog };
    }
    case "Battle": {
      const afterBattle = runBattle(newState, action.enemy, afterEffectLog, idGen);
      if (afterBattle.newState === "invalid") {
        return { newState: "invalid", newLog: afterBattle.newLog };
      }
      return { newState: afterBattle.newState, newLog: afterBattle.newLog };
    }
    case "Recruit": {
      const id = idGen.newId();
      const idCrew = {...action.crew, ...{ id }};
      newState = focus(newState,
        over(x => x.crew, x => x.concat([idCrew]))
      );
      return { newState, newLog: afterEffectLog };
    }
    case "BattleTurn": {
      return { newState, newLog: afterEffectLog };
    }
    case "StartBattle": {
      return { newState, newLog: afterEffectLog };
    }
    case "EndBattle": {
      return { newState, newLog: afterEffectLog };
    }
    case "GainHP": {
      const addHP = action.type === "permanent"
        ? (c: IdCrew) => focus(c, over(x => x.hp, x => x + action.value))
        : (c: IdCrew) => focus(c, over(x => x.hpTemp, x => x + action.value))
        ;
      newState = onTargets(action.target, addHP, newState);
      return { newState, newLog: afterEffectLog };
    }
    case "GainAP": {
      const addAP = action.type === "permanent"
        ? (c: IdCrew) => focus(c, over(x => x.ap, x => x + action.value))
        : (c: IdCrew) => focus(c, over(x => x.apTemp, x => x + action.value))
        ;
      newState = onTargets(action.target, addAP, newState);
      return { newState, newLog: afterEffectLog };
    }
    case "GainGold": {
      newState = focus(newState, over(x => x.gold, x => x + action.gain));
      return { newState, newLog: afterEffectLog };
    }
    case "PayGold": {
      if (newState.gold < action.pay) {
        return { newState: "invalid", newLog: afterEffectLog };
      } else {
        newState = focus(newState, over(x => x.gold, x => x - action.pay));
        return { newState, newLog: afterEffectLog };
      }
    }
    case "Death": {
      const index = indexOfId(action.targetId, newState.crew);
      if (index === "notFound") {
        throw ("index " + index + " not found");
      } else {
        newState = focus(newState, set(x => x.crew, newState.crew.slice(0, index).concat(newState.crew.slice(index + 1))));
        return { newState, newLog: afterEffectLog };
      }
    }
    case "AddItem": {
      newState = focus(newState, over(x => x.items, x => x.concat([action.item])));
      return { newState, newLog: afterEffectLog };
    }
    case "Rest": {
      return { newState, newLog: afterEffectLog };
    }
  }
}