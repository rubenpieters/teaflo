import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import { GameState } from "src/shared/game/state";
import { Enemy, runBattle } from "src/shared/game/enemy";

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

export type GainHP = {
  tag: "GainHP",
  target: "self",
  value: number,
}

export type GainAP = {
  tag: "GainAP",
  target: "self",
  value: number,
}

export type BattleTurn = {
  tag: "BattleTurn",
  turn: number,
}

export type Action
  = Recruit
  | Battle
  | Damage
  | BattleTurn
  | GainHP
  | GainAP

export function doAction(
  action: Action | Rest,
  state: GameState,
  log: (Action | Rest)[],
  from: number,
): { newState: GameState | "invalid", newLog: (Action | Rest)[] } {
  let newState: GameState = state;
  let newLog: (Action | Rest)[] = log;

  // crew interactions with effects
  for (const ally of state.crew.slice(from)) {
    for (const trigger of ally.triggers) {
      if (trigger.onTag === action.tag && trigger.type === "before") {
        const afterTrigger = doAction(trigger.action, newState, newLog, from + 1);
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
      let resultCrew: Crew[] = newState.crew;
      for (const position in action.positions) {
        const allyAtPos: Crew | undefined = newState.crew[position];
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
      newState = focus(newState,
        over(x => x.crew, x => [action.crew].concat(x))
      );
      return { newState, newLog: afterEffectLog };
    }
    case "BattleTurn": {
      return { newState, newLog: afterEffectLog };
    }
    case "GainHP": {
      return { newState, newLog: afterEffectLog };
    }
    case "GainAP": {
      return { newState, newLog: afterEffectLog };
    }
  }
}