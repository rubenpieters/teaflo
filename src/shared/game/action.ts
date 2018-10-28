import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";
import * as _Crew from "src/shared/game/crew";
import { GameState, IdCrew, IdEnemy, IdItem, CreatureId, onCreature, toGlobalId, IdInstance, entityExists, toPositionId, findEntity, onCreatures, PositionIdA } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";
import * as _Enemy from "src/shared/game/enemy";
import { Generator } from "src/shared/handler/id/generator";
import { Origin, TargetType, indexOfId } from "src/shared/game/target";
import { Item } from "src/shared/game/item";
import { Status } from "src/shared/game/status";
import * as _Status from "src/shared/game/status";
import { StatusLog, ApplyActionLog } from "src/shared/game/log";
import { Instance } from "./instance";
import { Transform, StatusTag, TransformTag } from "src/shared/game/status";

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
  | StartTurn
  | LoseFragment
  | SetHP
  | Abort
  ;

export type Damage = {
  tag: "Damage",
  target: CreatureId,
  value: number,
  piercing: boolean,
};

export type Heal = {
  tag: "Heal",
  target: CreatureId,
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
  target: CreatureId,
  value: number,
};

export type GainAP = {
  tag: "GainAP",
  target: CreatureId,
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
  target: CreatureId,
  status: Status | Transform,
};

export type AddStatus = {
  tag: "AddStatus",
  target: CreatureId,
  status: Status | Transform,
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
  target: CreatureId,
  status: Status["tag"],
};

export type Invalid = {
  tag: "Invalid",
};

export type Abort = {
  tag: "Abort",
};

export type ChargeUse = {
  tag: "ChargeUse",
  target: CreatureId,
  value: number,
};

export type AddThreat = {
  tag: "AddThreat",
  target: CreatureId,
  value: number,
  enemyId: CreatureId,
};

export type StartTurn = {
  tag: "StartTurn",
};

export type LoseFragment = {
  tag: "LoseFragment",
  target: CreatureId,
  type: StatusTag | TransformTag,
  value: number,
};

export type SetHP = {
  tag: "SetHP",
  target: CreatureId,
  value: number,
};

export function enemyTurn(
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
): { state: GameState | "invalid", log: ApplyActionLog }  {
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

export function applyTransforms(
  action: Action,
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ApplyActionLog, action: Action } {
  let i = 0;
  for (const enemy of state.enemies) {
    const afterStatus = _Status.checkTransforms(
      action, enemy, state, { tag: "PositionId", type: "enemy", id: i }, log, idGen, origin
    );
    if (afterStatus.state === "invalid") {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    action = afterStatus.action;
    i += 1;
  }
  i = 0;
  for (const ally of state.crew) {
    const afterStatus = _Status.checkTransforms(
      action, ally, state, { tag: "PositionId", type: "ally", id: i }, log, idGen, origin
    );
    if (afterStatus.state === "invalid") {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    action = afterStatus.action;
    i += 1;
  }
  return { state, log, action };
}

export function applyActionAndTriggers(
  action: Action,
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ApplyActionLog } {
  const afterTransforms = applyTransforms(action, state, log, idGen, origin);
  if (afterTransforms.state === "invalid") {
    return afterTransforms;
  }
  state = afterTransforms.state;
  log = afterTransforms.log;
  action = afterTransforms.action;

  let i = 0;
  for (const enemy of state.enemies) {
    const afterStatus = _Status.checkStatus(
      action, enemy, state, { tag: "PositionId", type: "enemy", id: i }, log, idGen, origin
    );
    if (afterStatus.state === "invalid" || afterStatus.abort) {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    i += 1;
  }
  i = 0;
  for (const ally of state.crew) {
    const afterStatus = _Status.checkStatus(
      action, ally, state, { tag: "PositionId", type: "ally", id: i }, log, idGen, origin
    );
    if (afterStatus.state === "invalid" || afterStatus.abort) {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    i += 1;
  }
  const afterApply = applyAction(action, state, origin, log, idGen);
  return afterApply;
}

// applies the results of this action to the state
function applyAction(
  action: Action,
  state: GameState,
  origin: Origin,
  log: ApplyActionLog,
  idGen: Generator,
): { state: GameState | "invalid", log: ApplyActionLog } {
  log = log.concat({ tag: "ApplyAction", action });

  switch (action.tag) {
    case "AddEnemy": {
      const id = idGen.newId();
      const addedEnemy: IdEnemy = {...action.enemy, ...{ id, actionIndex: 0, tag: "enemy", status: [] } };
      state = focus(state, over(x => x.enemies, x => x.concat(addedEnemy)));
      break;
    }
    case "AddCrew": {
      if (state.crew.length >= state.crewLimit) {
        return { state: "invalid", log };
      } else {
        const id = idGen.newId();
        const addedCrew: IdCrew = {...action.crew, ...{ id, actionIndex: 0, tag: "ally", status: [] } };
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
      if (! entityExists(action.target, state)) {
        return { state: "invalid", log };
      }
      // apply damage
      state = onCreature(action.target, state,
        ally => _Crew.damage(ally, action.value, action.piercing),
        enemy => _Enemy.damage(enemy, action.value, action.piercing),
      );
      // create threat
      // TODO: invoke AddThreat action?
      if (origin !== "noOrigin" && origin.type === "ally" && action.target.type === "enemy") {
        const originPosition = toPositionId(state, origin).id;
        state = focus(state,
          over(x => x.crew[originPosition], x => _Crew.addThreat(state, x, action.value, action.target)),
        );
      }
      break;
    }
    case "Heal": {
      state = onCreature(action.target, state,
        ally => _Crew.heal(ally, action.value),
        enemy => _Enemy.heal(enemy, action.value),
      );
      break;
    }
    case "GainHP": {
      state = onCreature(action.target, state,
        ally => _Crew.addHP(ally, action.value),
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "GainAP": {
      state = onCreature(action.target, state,
        ally => _Crew.addAP(ally, action.value),
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
      /*const meleeCrew: IdCrew | undefined = state.crew[0];
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
      }*/
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
        over(x => x.statusQueue, x => x.concat({ action: newAction, origin: <any>undefined })),
      );
      break;
    }
    case "AddStatus": {
      if (action.status.tag === "Mark") {
        const e = findEntity(state, action.target);
        if (_Status.findStatus(e, "Mark") === undefined) {
          state = onCreatures(action.target.type, state,
            ally => _Status.removeStatus(ally, "Mark"),
            enemy => _Status.removeStatus(enemy, "Mark"),
          );
        }
        state = onCreature(action.target, state,
          ally => _Status.addStatusTransform(ally, action.status),
          enemy => _Status.addStatusTransform(enemy, action.status),
        );
      } else {
        state = onCreature(action.target, state,
          ally => _Status.addStatusTransform(ally, action.status),
          enemy => _Status.addStatusTransform(enemy, action.status),
        );
      }
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
      throw "TODO";
      /*state = onCreature(action.target, state,
        ally => _Status.clearStatus(ally, action.status),
        enemy => _Status.clearStatus(enemy, action.status),
      );*/
    }
    case "ChargeUse": {
      if (action.target.type === "enemy") {
        const targetPosition = toPositionId(state, action.target).id;
        if (state.enemies[targetPosition].charges < action.value) {
          return { state: "invalid", log };
        }
      } else if (action.target.type === "ally") {
        const targetPosition = toPositionId(state, action.target).id;
        if (state.crew[targetPosition].charges < action.value) {
          return { state: "invalid", log };
        }
      } else {
        throw `wrong target type (${action.target.type}) for ${action.tag}`
      }
      state = onCreature(action.target, state,
        ally => _Crew.useCharge(ally, action.value),
        enemy => _Crew.useCharge(enemy, action.value),
      );
      break;
    }
    case "AddThreat": {
      state = onCreature(action.target, state,
        ally => _Crew.addThreat(state, ally, action.value, action.enemyId),
        _ => { throw `wrong target type for '${action.tag}`; },
      );
      break;
    }
    case "LoseFragment": {
      state = onCreature(action.target, state,
        ally => _Status.loseFragments(ally, action.type, action.value),
        enemy => _Status.loseFragments(enemy, action.type, action.value),
      );
      break;
    }
    case "SetHP": {
      state = onCreature(action.target, state,
        ally => _Crew.setHP(ally, action.value),
        enemy =>_Crew.setHP(enemy, action.value),
      );
      break;
    }
    case "StartTurn": {
      break;
    }
    case "Invalid": {
      return { state: "invalid", log };
    }
    case "Abort": {
      break;
    }
  }

  return { state, log };
}

export function checkDeaths(
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
): { state: GameState | "invalid", log: ApplyActionLog } {

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

export function applyActionQueue(
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
): { state: GameState | "invalid", log: ApplyActionLog } {
  for (const { action, origin } of state.statusQueue) {
    const afterApply = applyActionAndTriggers(action, state, log, idGen, origin);
    log = afterApply.log;
    if (afterApply.state === "invalid") {
      return { state: "invalid", log };
    }
    state = afterApply.state;
  }
  state = focus(state, set(x => x.statusQueue, []));
  return { state, log };
}
