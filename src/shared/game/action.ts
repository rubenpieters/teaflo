import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import * as _Crew from "src/shared/game/crew";
import { GameState, IdCrew, IdEnemy, Id, IdItem } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";
import * as _Enemy from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Target, TargetSpec, onTarget, determineTarget, TargetType, indexOfId, typeColl } from "src/shared/game/target";
import { Item } from "src/shared/game/item";
import { Status, HasStatus } from "src/shared/game/status";
import * as _Status from "src/shared/game/status";
import { checkConditions } from "src/shared/game/trigger";

// Action

export type Damage<T> = {
  tag: "Damage",
  target: T,
  value: number,
};

export type Heal<T> = {
  tag: "Heal",
  target: T,
  value: number,
};

export type AddEnemy = {
  tag: "AddEnemy",
  enemy: Enemy,
};

export type AddCrew = {
  tag: "AddCrew",
  crew: Crew,
};

export type AddItem = {
  tag: "AddItem",
  item: Item,
};

export type GainHP<T> = {
  tag: "GainHP",
  target: T,
  value: number,
};

export type GainAP<T> = {
  tag: "GainAP",
  target: T,
  value: number,
};

export type DamageAP<T> = {
  tag: "DamageAP",
  target: T,
  value: number,
};

export type Rest = {
  tag: "Rest",
};

export type GainGold = {
  tag: "GainGold",
  gain: number,
};

export type PayGold = {
  tag: "PayGold",
  pay: number,
};

export type BattleTurn = {
  tag: "BattleTurn",
};

export type Death = {
  tag: "Death",
  id: number,
  type: TargetType,
};

export type AddStatus<T> = {
  tag: "AddStatus",
  target: T,
  status: Status,
};

export type Noop = {
  tag: "Noop",
};

export type Action<T>
  = Damage<T>
  | Heal<T>
  | AddEnemy
  | AddCrew
  | AddItem
  | GainHP<T>
  | GainAP<T>
  | DamageAP<T>
  | Rest
  | GainGold
  | PayGold
  | BattleTurn
  | Death
  | AddStatus<T>
  | Noop
  ;

// Spec


export type ApDamage<T> = {
  tag: "ApDamage",
  target: T,
  multiplier: number,
};

export type Spec<T>
  = Action<T>
  | ApDamage<T>
  ;

function determineSpec(
  action: ActionSpec,
  state: GameState,
  selfId: number,
  selfType: TargetType,
): Action<TargetSpec> {
  switch (action.tag) {
    case "ApDamage": {
      if (selfType !== "ally") {
        throw "Wrong self type for action " + action.tag + ", was: " + selfType;
      }
      const self: Crew = state.crew[selfId];
      return {
        tag: "Damage",
        target: action.target,
        value: action.multiplier * self.ap,
      };
    }
    default: return action;
  }
}

export type ActionTarget = Action<Target>;

export type ActionSpec = Spec<TargetSpec>;

export function fmap<A, B>(
  f: (a: A) => B,
  action: Action<A>,
): Action<B> {
  switch (action.tag) {
    case "Damage": return {...action, ...{ target: f(action.target)}};
    case "Heal": return {...action, ...{ target: f(action.target)}};
    case "AddEnemy": return action;
    case "AddCrew": return action;
    case "AddItem": return action;
    case "GainHP": return {...action, ...{ target: f(action.target)}};
    case "GainAP": return {...action, ...{ target: f(action.target)}};
    case "DamageAP": return {...action, ...{ target: f(action.target)}};
    case "Rest": return action;
    case "GainGold": return action;
    case "PayGold": return action;
    case "BattleTurn": return action;
    case "Death": return action;
    case "AddStatus": return {...action, ...{ target: f(action.target)}};
    case "Noop": return action;
  }
}

export function enemyTurn(
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  let acc: GameState | "invalid" = state;
  let i = 0;
  for (const enemy of state.enemies) {
    if (acc === "invalid") {
      return { state: "invalid", log };
    } else {
      const afterEnemy = _Enemy.act(enemy, acc, log, idGen, i);
      acc = afterEnemy.state;
      log = afterEnemy.log;
    }
    i += 0;
  }
  return { state: acc, log };
}

export function determineAndApplyActionAndTriggers(
  action: ActionSpec,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
  selfId: number,
  selfType: TargetType,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  const actionSpec = determineSpec(action, state, selfId, selfType);
  const actionTarget = fmap(x => determineTarget(x, state, selfId, selfType), actionSpec);
  return determineAndApplyActionAndTriggersAt(action, state, log, { id: 0, type: "enemy" }, idGen, selfId, selfType);
}

export function determineAndApplyActionAndTriggersAt(
  action: ActionSpec,
  state: GameState,
  log: ActionTarget[],
  from: { id: number, type: "item" | "crew" | "enemy" },
  idGen: Generator,
  selfId: number,
  selfType: TargetType,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  const actionSpec = determineSpec(action, state, selfId, selfType);
  const actionTarget = fmap(x => determineTarget(x, state, selfId, selfType), actionSpec);
  return applyActionAndTriggersAt(actionTarget, state, log, from, idGen);
}

export function applyActionAndTriggers(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  return applyActionAndTriggersAt(action, state, log, { id: 0, type: "enemy" }, idGen);
}

function applyActionAndTriggersAt(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  from: { id: number, type: "item" | "crew" | "enemy" },
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  // enemy interactions with effects
  if (from.type === "enemy") {
    for (const enemy of state.enemies.slice(from.id)) {
      for (const trigger of enemy.triggers) {
        if (trigger.onTag === action.tag && trigger.type === "before" && checkConditions(trigger.conditions, action, state, enemy.id, "enemy")) {
          const afterTrigger = determineAndApplyActionAndTriggersAt(
            trigger.action, state, log, { id: from.id + 1, type: "enemy" }, idGen, enemy.id, "enemy");
          if (afterTrigger.state === "invalid") {
            return afterTrigger;
          }
          state = afterTrigger.state;
          log = afterTrigger.log;
        }
      }
    }
  }

  // item interactions with effects
  if (from.type === "item" || from.type === "enemy") {
    const startId = from.type === "item" ? from.id : 0;
    for (const item of state.items.slice(startId)) {
      for (const trigger of item.triggers) {
        if (trigger.onTag === action.tag && trigger.type === "before" && checkConditions(trigger.conditions, action, state, item.id, "item")) {
          const afterTrigger = determineAndApplyActionAndTriggersAt(trigger.action, state, log, { id: from.id + 1, type: "item" }, idGen, item.id, "item");
          if (afterTrigger.state === "invalid") {
            return afterTrigger;
          }
          state = afterTrigger.state;
          log = afterTrigger.log;
        }
      }
    }
  }

  const fromCrew = from.type === "crew" ? from.id : 0;
  // crew interactions with effects
  for (const ally of state.crew.slice(fromCrew)) {
    for (const trigger of ally.triggers) {
      if (trigger.onTag === action.tag && trigger.type === "before" && checkConditions(trigger.conditions, action, state, ally.id, "ally")) {
        const afterTrigger = determineAndApplyActionAndTriggersAt(trigger.action, state, log, { id: from.id + 1, type: "crew" }, idGen, ally.id, "ally");
        if (afterTrigger.state === "invalid") {
          return afterTrigger;
        }
        state = afterTrigger.state;
        log = afterTrigger.log;
      }
    }
  }

  const afterApply = applyAction(action, state, log, idGen);

  return afterApply;
}

// applies the results of this action to the state
function applyAction(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  console.log(action.tag);
  log = log.concat(action);

  switch (action.tag) {
    case "AddEnemy": {
      const id = idGen.newId();
      const addedEnemy: IdEnemy = {...action.enemy, ...{ id, actionIndex: 0, tag: "enemy" } };
      state = focus(state, over(x => x.enemies, x => x.concat(addedEnemy)));
      break;
    }
    case "AddCrew": {
      const id = idGen.newId();
      const addedCrew: IdCrew = {...action.crew, ...{ id, actionIndex: 0, tag: "ally" } };
      state = focus(state, over(x => x.crew, x => x.concat(addedCrew)));
      break;
    }
    case "AddItem": {
      const id = idGen.newId();
      const addedItem: IdItem = {...action.item, ...{ id, tag: "item" } };
      state = focus(state, over(x => x.items, x => x.concat(addedItem)));
      break;
    }
    case "Damage": {
      // if not all positions exists, then invalid solution
      if (Math.max(...action.target.positions) >= typeColl(state, action.target.type).length) {
        return { state: "invalid", log };
      }
      // apply damage
      state = onTarget(action.target, state,
        ally => _Crew.damage(ally, action.value),
        enemy => _Enemy.damage(enemy, action.value),
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "Heal": {
      state = onTarget(action.target, state,
        ally => _Crew.heal(ally, action.value),
        enemy => _Enemy.heal(enemy, action.value),
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "GainHP": {
      state = onTarget(action.target, state,
        ally => _Crew.addHP(ally, action.value),
        x => { throw "wrong target type for '" + action.tag + "'"; },
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "DamageAP": {
      state = onTarget(action.target, state,
        ally => _Crew.damageAP(ally, action.value),
        x => { throw "wrong target type for '" + action.tag + "'"; },
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "DamageAP": {
      state = onTarget(action.target, state,
        ally => _Crew.addAP(ally, action.value),
        x => { throw "wrong target type for '" + action.tag + "'"; },
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "Rest": {
      break;
    }
    case "GainGold": {
      state = focus(state, over(x => x.gold, x => x + action.gain));
      break;
    }
    case "PayGold": {
      if (state.gold < action.pay) {
        return { state: "invalid", log };
      }
      state = focus(state, over(x => x.gold, x => x - action.pay));
      break;
    }
    case "BattleTurn": {
      const meleeCrew: IdCrew | undefined = state.crew[0];
      const def = { state, log };
      const afterMelee = meleeCrew === undefined ? def : _Crew.act(meleeCrew, state, log, idGen, 0);
      if (afterMelee.state === "invalid") {
        return afterMelee;
      }
      state = afterMelee.state;
      log = afterMelee.log;

      let i = 1;
      for (const rangedCrew of state.crew.slice(1)) {
        if (rangedCrew.ranged) {
          const afterRanged = _Crew.act(rangedCrew, state, log, idGen, i);
          if (afterRanged.state === "invalid") {
            return afterRanged;
          }
          state = afterRanged.state;
          log = afterRanged.log;
        }
        i += 1;
      }
      break;
    }
    case "Death": {
      switch (action.type) {
        case "ally": {
          const index = indexOfId(action.id, state.crew);
          if (index === "notFound") {
            throw ("index " + action.id + " not found");
          } else {
            state = focus(state,
              set(x => x.crew, state.crew.slice(0, index).concat(state.crew.slice(index + 1)))
            );
          }
          break;
        }
        case "enemy": {
          const index = indexOfId(action.id, state.enemies);
          if (index === "notFound") {
            throw ("index " + action.id + " not found");
          } else {
            state = focus(state,
              set(x => x.enemies, state.enemies.slice(0, index).concat(state.enemies.slice(index + 1)))
            );
          }
          break;
        }
        case "item": {
          const index = indexOfId(action.id, state.items);
          if (index === "notFound") {
            throw ("index " + action.id + " not found");
          } else {
            state = focus(state,
              set(x => x.items, state.items.slice(0, index).concat(state.items.slice(index + 1)))
            );
          }
          break;
        }
      }
      break;
    }
    case "AddStatus": {
      state = onTarget(action.target, state,
        ally => _Status.addStatus(ally, action.status),
        enemy => _Status.addStatus(enemy, action.status),
        x => { throw "wrong target type for '" + action.tag + "'"; },
      );
      break;
    }
    case "Noop": {
      break;
    }
  }

  return { state, log };
}

export function checkDeaths(
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {

  for (const ally of state.crew) {
    if (ally.hp <= 0) {
      const deathAction: ActionTarget = { tag: "Death", type: "ally", id: ally.id };
      const afterDeath = applyActionAndTriggers(deathAction, state, log, idGen);
      if (afterDeath.state === "invalid") {
        return afterDeath;
      }
      state = afterDeath.state;
      log = afterDeath.log;
    }
  }

  for (const enemy of state.enemies) {
    if (enemy.hp <= 0) {
      const deathAction: ActionTarget = { tag: "Death", type: "enemy", id: enemy.id };
      const afterDeath = applyActionAndTriggers(deathAction, state, log, idGen);
      if (afterDeath.state === "invalid") {
        return afterDeath;
      }
      state = afterDeath.state;
      log = afterDeath.log;
    }
  }

  return { state, log };
}

export function checkStatusEnemy(
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  let i = 0;
  for (const enemy of state.enemies) {
    for (const statusTag of _Status.allStatus) {
      const status = enemy[statusTag];
      if (status !== undefined) {
        const action = _Status.statusToAction(status);
        const afterApply = determineAndApplyActionAndTriggers(action, state, log, idGen, enemy.id, "enemy");
        if (afterApply.state === "invalid") {
          return afterApply;
        }
        state = focus(afterApply.state,
          over(x => x.enemies[i], x => _Status.applyStatus(i, x, statusTag)),
        );
        log = afterApply.log;
      }
    }
    i += 1;
  }

  return { state, log };
}

export function checkStatusCrew(
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  let i = 0;
  for (const ally of state.crew) {
    for (const statusTag of _Status.allStatus) {
      const status = ally[statusTag];
      if (status !== undefined) {
        const action = _Status.statusToAction(status);
        const afterApply = determineAndApplyActionAndTriggers(action, state, log, idGen, ally.id, "ally");
        if (afterApply.state === "invalid") {
          return afterApply;
        }
        state = focus(afterApply.state,
          set(x => x.crew[i], _Status.applyStatus(i, ally, statusTag)),
        );
        log = afterApply.log;
      }
    }
    i += 1;
  }

  return { state, log };
}

/*

export type Damage<T> = {
  tag: "Damage",
  target: T,
  value: number,
};

export type AddEnemy = {
  tag: "AddEnemy",
  enemy: Enemy,
};

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
  = Damage<T>
  | AddEnemy
  | Recruit
  | Battle
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
    case "Damage": return {...action, ...{ target: f(action.target)}};
    case "AddEnemy": return action;
    case "Recruit": return action;
    case "Battle": return action;
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

// applies the results of this action
function applyAction(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { state: GameState | "invalid", log: ActionTarget[] } {
  log = log.concat(action);

  switch (action.tag) {
    case "AddEnemy": {
      const id = idGen.newId();
      const addedEnemy = {...action.enemy, ...{ id }}
      state = focus(state, over(x => x.enemies, x => x.concat(addedEnemy)));
      return { state, log };
    }
    case "Damage": {
      // action.target

      return { state, log };
    }
    default:
      throw "unimplemented";
  }
}

export type ActionTarget = Action<Target>;

export function doAction(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionTarget[] } {
  return doActionAt(action, state, log, { id: 0, type: "item" }, idGen);
}

export function doActionAt(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  from: { id: number, type: "item" | "crew" },
  idGen: Generator,
): { newState: GameState | "invalid", newLog: ActionTarget[] } {
  let newState: GameState = state;
  let newLog: ActionTarget[] = log;

  // item interactions with effects
  if (from.type === "item") {
    for (const item of state.items.slice(from.id)) {
      for (const trigger of item.triggers) {
        if (trigger.onTag === action.tag && trigger.type === "before") {
          const action = fmap(x => findTarget(x, newState, item.id, "item"), trigger.action);
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
        const action = fmap(x => findTarget(x, newState, ally.id, "ally"), trigger.action);
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
      newState = onCrew(action.target, newState, addHP);
      return { newState, newLog: afterEffectLog };
    }
    case "GainAP": {
      const addAP = action.type === "permanent"
        ? (c: IdCrew) => focus(c, over(x => x.ap, x => x + action.value))
        : (c: IdCrew) => focus(c, over(x => x.apTemp, x => x + action.value))
        ;
      newState = onCrew(action.target, newState, addAP);
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
      const id = idGen.newId();
      const addedItem = {...action.item, ...{ id: id } };
      newState = focus(newState, over(x => x.items, x => x.concat(addedItem)));
      return { newState, newLog: afterEffectLog };
    }
    case "Rest": {
      return { newState, newLog: afterEffectLog };
    }
  }
}
*/