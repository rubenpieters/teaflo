import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import * as _Crew from "src/shared/game/crew";
import { GameState, IdCrew, IdEnemy, IdItem, CreatureId, toPositionId, toGlobalId, IdInstance } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";
import * as _Enemy from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Target, Origin, onTarget, TargetType, indexOfId, typeColl } from "src/shared/game/target";
import { Item } from "src/shared/game/item";
import { Status } from "src/shared/game/status";
import * as _Status from "src/shared/game/status";
import { StatusLog } from "src/shared/game/log";
import { Instance } from "./instance";

export type ActionSpec = (state: GameState, selfId: number, selfType: TargetType) => Action;

export type Action
  = Damage
  | Heal
  | Death
  | AddEnemy
  | AddCrew
  | AddItem
  | GainHP
  | GainAP
  | Rest
  | GainGold
  | PayGold
  | BattleTurn
  | Death
  | QueueStatus
  | AddStatus
  | Noop
  | Swap
  | CombinedAction
  | ClearStatus
  | Invalid
  | ChargeUse
  | AddInstance
  | AddThreat
  ;

export type Damage = {
  tag: "Damage",
  target: Target,
  value: number,
  piercing: boolean,
};

export type Heal = {
  tag: "Heal",
  target: Target,
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

export type AddInstance = {
  tag: "AddInstance",
  instance: Instance,
  team: "ally" | "enemy",
};

export type AddItem = {
  tag: "AddItem",
  item: Item,
};

export type GainHP = {
  tag: "GainHP",
  target: Target,
  value: number,
};

export type GainAP = {
  tag: "GainAP",
  target: Target,
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

export type QueueStatus = {
  tag: "QueueStatus",
  target: Target,
  status: Status,
};

export type AddStatus = {
  tag: "AddStatus",
  target: Target,
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

export type CombinedAction = {
  tag: "CombinedAction",
  actions: Action[],
};

export type ClearStatus = {
  tag: "ClearStatus",
  target: Target,
  status: Status["tag"],
};

export type Invalid = {
  tag: "Invalid",
};

export type ChargeUse = {
  tag: "ChargeUse",
  target: Target,
  value: number,
};

export type AddThreat = {
  tag: "AddThreat",
  target: Target,
  value: number,
  enemyId: number,
};

export function enemyTurn(
  state: GameState,
  log: Action[],
  idGen: Generator,
): { state: GameState | "invalid", log: Action[] }  {
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

export function highestThreatTarget(
  enemyId: CreatureId,
  state: GameState,
): { target: Crew, position: number } | undefined {
  let enemyGlobalId = toGlobalId(state, enemyId).id;
  let highestThreat: { ally: Crew, position: number, threat: number } | undefined = undefined;
  let i = 0;
  for (const ally of state.crew) {
    const threat: number | undefined = ally.threatMap[enemyGlobalId];
    if (highestThreat === undefined) {
      highestThreat = { ally, position: i, threat: threat === undefined ? 0 : threat };
    } else if (threat !== undefined && highestThreat.threat < threat) {
      highestThreat = { ally, position: i, threat }
    }
    i += 1;
  }
  return highestThreat === undefined ? undefined : { target: highestThreat.ally, position: highestThreat.position };
}

export function applyActionAndTriggers(
  action: Action,
  state: GameState,
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  return applyActionAndTriggersAt(action, state, log, { id: 0, type: "enemy" }, idGen, origin);
}

// TODO: checking tags can be done in triggers themselves?

function applyActionAndTriggersAt(
  action: Action,
  state: GameState,
  log: Action[],
  from: { id: number, type: "item" | "crew" | "enemy" },
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  // enemy interactions with effects
  if (from.type === "enemy") {
    let indexEnemy = 0;
    for (const enemy of state.enemies.slice(from.id)) {
      let indexTrigger = 0;
      for (const trigger of enemy.triggers) {
        if (trigger.charges > 0 && trigger.type === "before") {
          const triggerResult = trigger.effect(action)(state, { tag: "GlobalId", id: enemy.id, type: "enemy" });
          state = focus(state,
            over(x => x.enemies[indexEnemy].triggers[indexTrigger].charges, x => x - triggerResult.chargeUse),
          );
          const afterTrigger = applyActionAndTriggersAt(
            triggerResult.action, state, log, { id: from.id + 1, type: "enemy" }, idGen, origin);
          if (afterTrigger.state === "invalid") {
            return afterTrigger;
          }
          state = afterTrigger.state;
          log = afterTrigger.log;
        } else if (trigger.charges > 0 && trigger.type === "instead") {
          const triggerResult = trigger.effect(action)(state, { tag: "GlobalId", id: enemy.id, type: "enemy" });
          state = focus(state,
            over(x => x.enemies[indexEnemy].triggers[indexTrigger].charges, x => x - triggerResult.chargeUse),
          );
          action = triggerResult.action;
        }
        indexTrigger += 1;
      }
      state = focus(state,
        set(x => x.enemies[indexEnemy].triggers, state.enemies[indexEnemy].triggers.filter(v => v.charges > 0)),
      );
      indexEnemy += 1;
    }
  }

  // item interactions with effects
  if (from.type === "item" || from.type === "enemy") {
    const startId = from.type === "item" ? from.id : 0;
    let indexItem = startId;
    for (const item of state.items.slice(startId)) {
      let indexTrigger = 0;
      for (const trigger of item.triggers) {
        if (trigger.charges > 0 && trigger.onTag === action.tag && trigger.type === "before") {
          const triggerResult = trigger.action(action)(state, item.id, "item");
          state = focus(state,
            over(x => x.items[indexItem].triggers[indexTrigger].charges, x => x - triggerResult.charges),
          );
          const afterTrigger = applyActionAndTriggersAt(
            triggerResult.action, state, log, { id: from.id + 1, type: "item" }, idGen, origin);
          if (afterTrigger.state === "invalid") {
            return afterTrigger;
          }
          state = afterTrigger.state;
          log = afterTrigger.log;
        } else if (trigger.charges > 0 && trigger.onTag === action.tag && trigger.type === "instead") {
          const triggerResult = trigger.action(action)(state, item.id, "item");
          state = focus(state,
            over(x => x.items[indexItem].triggers[indexTrigger].charges, x => x - triggerResult.charges),
          );
          action = triggerResult.action;
        }
      }
      state = focus(state,
        set(x => x.items[indexItem].triggers, state.items[indexItem].triggers.filter(v => v.charges > 0)),
      );
      indexItem += 1;
    }
  }

  const fromCrew = from.type === "crew" ? from.id : 0;
  // crew interactions with effects
  let indexAlly = fromCrew;
  for (const ally of state.crew.slice(fromCrew)) {
    let indexTrigger = 0;
    for (const trigger of ally.triggers) {
      if (trigger.charges > 0 && trigger.type === "before") {
        const triggerResult = trigger.effect(action)(state, { tag: "PositionId", id: ally.id, type: "ally" });
        state = focus(state,
          over(x => x.crew[indexAlly].triggers[indexTrigger].charges, x => x - triggerResult.chargeUse),
        );
        const afterTrigger = applyActionAndTriggersAt(
          triggerResult.action, state, log, { id: from.id + 1, type: "crew" }, idGen, origin);
        if (afterTrigger.state === "invalid") {
          return afterTrigger;
        }
        state = afterTrigger.state;
        log = afterTrigger.log;
      } else if (trigger.charges > 0 && trigger.type === "instead") {
        const triggerResult = trigger.effect(action)(state, { tag: "PositionId", id: ally.id, type: "ally" });
        state = focus(state,
          over(x => x.crew[indexAlly].triggers[indexTrigger].charges, x => x - triggerResult.chargeUse),
        );
        action = triggerResult.action;
      }
    }
    state = focus(state,
      set(x => x.crew[indexAlly].triggers, state.crew[indexAlly].triggers.filter(v => v.charges > 0)),
    );
    indexAlly += 1;
  }

  const afterApply = applyAction(action, state, origin, log, idGen);

  return afterApply;
}

// applies the results of this action to the state
function applyAction(
  action: Action,
  state: GameState,
  origin: Origin,
  log: Action[],
  idGen: Generator,
): { state: GameState | "invalid", log: Action[] } {
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
    case "AddInstance": {
      const id = idGen.newId();
      const addedInstance: IdInstance = {...action.instance, ...{ id } };
      if (action.team === "ally") {
        state = focus(state, over(x => x.allyInstances, x => x.concat(addedInstance)));
      } else if (action.team === "enemy") {
        state = focus(state, over(x => x.enemyInstances, x => x.concat(addedInstance)));
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
      // if position does not exist, then invalid solution
      if (action.target.position >= typeColl(state, action.target.type).length) {
        return { state: "invalid", log };
      }
      // apply damage
      state = onTarget(action.target, state,
        ally => _Crew.damage(ally, action.value, action.piercing),
        enemy => _Enemy.damage(enemy, action.value, action.piercing),
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      // create threat
      // TODO: invoke AddThreat action?
      if (origin !== "noOrigin" && origin.type === "ally" && action.target.type === "enemy") {
        state = focus(state,
          over(x => x.crew[origin.id], x => _Crew.addThreat(x, action.value, state.enemies[action.target.position].id))
        );
      }
      break;
    }
    case "Heal": {
      state = onTarget(action.target, state,
        ally => _Crew.heal(ally, action.value),
        enemy => _Enemy.heal(enemy, action.value),
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "GainHP": {
      state = onTarget(action.target, state,
        ally => _Crew.addHP(ally, action.value),
        _ => { throw `wrong target type for '${action.tag}`; },
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "GainAP": {
      state = onTarget(action.target, state,
        ally => _Crew.addAP(ally, action.value),
        _ => { throw `wrong target type for '${action.tag}`; },
        _ => { throw `wrong target type for '${action.tag}`; },
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
            throw (`index ${action.id} not found`);
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
            throw (`index ${action.id} not found`);
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
            throw (`index ${action.id} not found`);
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
    case "QueueStatus": {
      const newAction: Action = {...action, tag: "AddStatus"}
      state = focus(state,
        // TODO: pass correct origin
        over(x => x.actionQueue, x => x.concat({ action: newAction, origin: <any>undefined })),
      );
      break;
    }
    case "AddStatus": {
      state = onTarget(action.target, state,
        ally => _Status.addStatus(ally, action.status),
        enemy => _Status.addStatus(enemy, action.status),
        _ => { throw `wrong target type for '${action.tag}`; },
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
    case "CombinedAction": {
      for (const embeddedAction of action.actions) {
        const result = applyAction(embeddedAction, state, origin, log, idGen);
        log = result.log;
        if (result.state === "invalid") {
          return { state: "invalid", log };
        }
        state = result.state;
      }
      break;
    }
    case "ClearStatus": {
      state = onTarget(action.target, state,
        ally => _Status.clearStatus(ally, action.status),
        enemy => _Status.clearStatus(enemy, action.status),
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "ChargeUse": {
      if (action.target.type !== "ally") {
        throw `wrong target type for ${action.tag}`
      }
      if (state.crew[action.target.position].charges < action.value) {
        return { state: "invalid", log };
      }
      state = onTarget(action.target, state,
        ally => _Crew.useCharge(ally, action.value),
        _ => { throw `wrong target type for '${action.tag}`; },
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "AddThreat": {
      state = onTarget(action.target, state,
        ally => _Crew.addThreat(ally, action.value, action.enemyId),
        _ => { throw `wrong target type for '${action.tag}`; },
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "Invalid": {
      return { state: "invalid", log };
    }
  }

  return { state, log };
}

export function checkDeaths(
  state: GameState,
  log: Action[],
  idGen: Generator,
): { state: GameState | "invalid", log: Action[] } {

  for (const ally of state.crew) {
    if (ally.hp <= 0) {
      const deathAction: Action = { tag: "Death", type: "ally", id: ally.id };
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
      const deathAction: Action = { tag: "Death", type: "enemy", id: enemy.id };
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
  idGen: Generator,
): { state: GameState | "invalid", log: StatusLog[] } {
  let log: StatusLog[] = [];
  let enemy: IdEnemy;
  for (let i = 0; i < state.enemies.length; i++) {
    for (const statusTag of _Status.allStatus) {
      enemy = state.enemies[i];
      const status = enemy[statusTag];
      if (status !== undefined) {
        const action = _Status.statusToAction(status, state, enemy.id, "enemy");
        state = focus(state,
          over(x => x.enemies[i], x => _Status.applyStatus(i, x, statusTag)),
        );
        const afterApply = applyActionAndTriggers(action, state, [], idGen, "noOrigin");
        log.push({ id: i, status: statusTag, actionLog: afterApply.log });
        if (afterApply.state === "invalid") {
          return { state: "invalid", log };
        }
        state = afterApply.state;
      }
    }
  }

  return { state, log };
}

export function checkStatusCrew(
  state: GameState,
  idGen: Generator,
): { state: GameState | "invalid", log: StatusLog[] } {
  let log: StatusLog[] = [];
  let ally: IdCrew;
  for (let i = 0; i < state.crew.length; i++) {
    for (const statusTag of _Status.allStatus) {
      ally = state.crew[i];
      const status = ally[statusTag];
      if (status !== undefined) {
        const action = _Status.statusToAction(status, state, ally.id, "ally");
        state = focus(state,
          set(x => x.crew[i], _Status.applyStatus(i, ally, statusTag)),
        );
        const afterApply = applyActionAndTriggers(action, state, [], idGen, "noOrigin");
        log.push({ id: i, status: statusTag, actionLog: afterApply.log });
        if (afterApply.state === "invalid") {
          return { state: "invalid", log };
        }
        state = afterApply.state;
      }
    }
  }

  return { state, log };
}

export function applyActionQueue(
  state: GameState,
  log: Action[],
  idGen: Generator,
): { state: GameState | "invalid", log: Action[] } {
  for (const { action, origin } of state.actionQueue) {
    const afterApply = applyActionAndTriggers(action, state, log, idGen, origin);
    log = afterApply.log;
    if (afterApply.state === "invalid") {
      return { state: "invalid", log };
    }
    state = afterApply.state;
  }
  state = focus(state, set(x => x.actionQueue, []));
  return { state, log };
}
