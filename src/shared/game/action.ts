import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import * as _Crew from "src/shared/game/crew";
import { GameState, IdCrew, IdEnemy, Id, IdItem } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";
import * as _Enemy from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Target, TargetSpec, Origin, onTarget, determineTarget, TargetType, indexOfId, typeColl } from "src/shared/game/target";
import { Item } from "src/shared/game/item";
import { Status, HasStatus } from "src/shared/game/status";
import * as _Status from "src/shared/game/status";
import { Condition, checkConditions } from "src/shared/game/trigger";

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

export type Swap = {
  tag: "Swap",
  type: TargetType,
  from: number,
  to: number,
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
  | Swap
  ;

// Spec


export type ApDamage<T> = {
  tag: "ApDamage",
  target: T,
  multiplier: number,
};

export type ConditionAction<T> = {
  tag: "ConditionAction",
  conditions: Condition[],
  trueAction: Spec<T>,
  falseAction: Spec<T>,
};

export type Spec<T>
  = Action<T>
  | ApDamage<T>
  | ConditionAction<T>
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
    case "ConditionAction": {
      // TODO: there is no sensible action to pass for checkConditions
      if (checkConditions(action.conditions, <any>undefined, state, selfId, selfType)) {
        return determineSpec(action.trueAction, state, selfId, selfType);
      } else {
        return determineSpec(action.falseAction, state, selfId, selfType);
      }
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
    case "Damage": return {...action, target: f(action.target)};
    case "Heal": return {...action, target: f(action.target)};
    case "AddEnemy": return action;
    case "AddCrew": return action;
    case "AddItem": return action;
    case "GainHP": return {...action, target: f(action.target)};
    case "GainAP": return {...action, target: f(action.target)};
    case "DamageAP": return {...action, target: f(action.target)};
    case "Rest": return action;
    case "GainGold": return action;
    case "PayGold": return action;
    case "BattleTurn": return action;
    case "Death": return action;
    case "AddStatus": return {...action, target: f(action.target)};
    case "Noop": return action;
    case "Swap": return action;
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
  origin: Origin,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  return determineAndApplyActionAndTriggersAt(action, state, log, { id: 0, type: "enemy" }, idGen, selfId, selfType, origin);
}

export function determineAndApplyActionAndTriggersAt(
  action: ActionSpec,
  state: GameState,
  log: ActionTarget[],
  from: { id: number, type: "item" | "crew" | "enemy" },
  idGen: Generator,
  selfId: number,
  selfType: TargetType,
  origin: Origin,
): { state: GameState | "invalid", log: ActionTarget[] }  {
  const actionSpec = determineSpec(action, state, selfId, selfType);
  const actionTarget = fmap(x => determineTarget(x, state, selfId, selfType, origin), actionSpec);
  return applyActionAndTriggersAt(actionTarget, state, log, from, idGen, origin);
}

export function applyActionAndTriggers(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ActionTarget[] } {
  return applyActionAndTriggersAt(action, state, log, { id: 0, type: "enemy" }, idGen, origin);
}

function applyActionAndTriggersAt(
  action: ActionTarget,
  state: GameState,
  log: ActionTarget[],
  from: { id: number, type: "item" | "crew" | "enemy" },
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ActionTarget[] } {
  // enemy interactions with effects
  if (from.type === "enemy") {
    for (const enemy of state.enemies.slice(from.id)) {
      for (const trigger of enemy.triggers) {
        if (trigger.onTag === action.tag && trigger.type === "before" && checkConditions(trigger.conditions, action, state, enemy.id, "enemy")) {
          const afterTrigger = determineAndApplyActionAndTriggersAt(
            trigger.action, state, log, { id: from.id + 1, type: "enemy" }, idGen, enemy.id, "enemy", origin);
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
          const afterTrigger = determineAndApplyActionAndTriggersAt(
            trigger.action, state, log, { id: from.id + 1, type: "item" }, idGen, item.id, "item", origin);
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
        const afterTrigger = determineAndApplyActionAndTriggersAt(
          trigger.action, state, log, { id: from.id + 1, type: "crew" }, idGen, ally.id, "ally", origin);
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
  log = log.concat(action);

  switch (action.tag) {
    case "AddEnemy": {
      const id = idGen.newId();
      const addedEnemy: IdEnemy = {...action.enemy, ...{ id, actionIndex: 0, tag: "enemy" } };
      state = focus(state, over(x => x.enemies, x => x.concat(addedEnemy)));
      break;
    }
    case "AddCrew": {
      if (state.crew.length >= state.crewLimit) {
        return { state: "invalid", log };
      } else {
        const id = idGen.newId();
        const addedCrew: IdCrew = {...action.crew, ...{ id, actionIndex: 0, tag: "ally" } };
        state = focus(state, over(x => x.crew, x => x.concat(addedCrew)));
      }
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
    case "Swap": {
      switch (action.type) {
        case "ally": {
          state = focus(state,
           set(x => x.crew[action.from], state.crew[action.to]),
           set(x => x.crew[action.to], state.crew[action.from]),
          );
          break;
        }
        case "enemy": {
          state = focus(state,
           set(x => x.enemies[action.from], state.enemies[action.to]),
           set(x => x.enemies[action.to], state.enemies[action.from]),
          );
          break;
        }
        case "item": {
          throw "swap not implemented for item";
        }
      }
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
      const afterDeath = applyActionAndTriggers(deathAction, state, log, idGen, "noOrigin");
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
      const afterDeath = applyActionAndTriggers(deathAction, state, log, idGen, "noOrigin");
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
        const action = _Status.statusToAction(status, enemy.id, "enemy");
        state = focus(state,
          over(x => x.enemies[i], x => _Status.applyStatus(i, x, statusTag)),
        );
        const afterApply = determineAndApplyActionAndTriggers(action, state, log, idGen, enemy.id, "enemy", "noOrigin");
        if (afterApply.state === "invalid") {
          return afterApply;
        }
        state = afterApply.state;
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
        const action = _Status.statusToAction(status, ally.id, "ally");
        state = focus(state,
          set(x => x.crew[i], _Status.applyStatus(i, ally, statusTag)),
        );
        const afterApply = determineAndApplyActionAndTriggers(action, state, log, idGen, ally.id, "ally", "noOrigin");
        if (afterApply.state === "invalid") {
          return afterApply;
        }
        state = afterApply.state;
        log = afterApply.log;
      }
    }
    i += 1;
  }

  return { state, log };
}