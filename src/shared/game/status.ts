import { focus, over, set } from "src/shared/iassign-util";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Origin, TargetType } from "src/shared/game/target";
import { findIndex } from "src/shared/game/trigger";
import { GameState, CreatureId, toPositionId, Id, findEntity } from "src/shared/game/state";
import { evStatic, evAnd, evAllies, evSelf, damage, addTarget, queueStatus, noTarget, chargeUse, heal, noop, evCondition, evTrigger, extra, addThreat, evEnemies, evStatusValue, guardTrigger, dmgBarrierTrigger, bubbleTrigger, weakTrigger, convertTrigger } from "src/shared/game/effectvar";
import { TriggerEntityEffect } from "src/shared/game/ability";
import { Generator } from "src/shared/handler/id/generator";
import * as EV from "src/shared/game/effectvar";
import { ApplyActionLog, ApplyActionLine } from "./log";

export type StatusTag = Status["tag"];
export type TransformTag = Transform["tag"];

export type Poison = {
  tag: "Poison",
  value: number,
  fragment: number,
};

export type PiercingPoison = {
  tag: "PiercingPoison",
  value: number,
  fragment: number,
};

export type Regen = {
  tag: "Regen",
  value: number,
  fragment: number,
};

export type Guard = {
  tag: "Guard",
  value: number,
  guard: number,
  fragment: number,
};

export type Doom = {
  tag: "Doom",
  value: number,
  fragment: number,
};

export type Blind = {
  tag: "Blind",
  value: number,
  fragment: number,
};

export type Silence = {
  tag: "Silence",
  value: number,
  fragment: number,
};

export type Bubble = {
  tag: "Bubble",
  value: 1,
  fragment: 0,
};

export type DmgBarrier = {
  tag: "DmgBarrier",
  damage: number,
  value: number,
  fragment: number,
}

export type Weak = {
  tag: "Weak",
  value: number,
  fragment: number,
};

export type Strong = {
  tag: "Strong",
  value: number,
  fragment: number,
};

export type Convert = {
  tag: "Convert",
  value: 1,
  fragment: 0,
};

export type Mark = {
  tag: "Mark",
  value: number,
  fragment: number,
};

export type Intercept = {
  tag: "Intercept",
  value: number,
  fragment: number,
};

export type Status
  = Poison
  | PiercingPoison
  | Regen
  | Doom
  | Blind
  | Silence
  | DmgBarrier
  | Mark
  | Intercept
  ;

export type Transform
  = Guard
  | Bubble
  | Weak
  | Convert
  | Strong
  ;

/*export function isStatus<A extends { tag: string }>(
  a: A,
): A is Status {
  if (
    a.tag === "Poison" ||
    a.tag === "PiercingPoison" ||
    a.tag === "Regen" ||
    a.tag === "Doom" ||
    a.tag === "Blind" ||
    a.tag === "Silence" ||
    a.tag === "DmgBarrier" ||
    a.tag === "Mark"
  ) {
    return true;
  } else {
    return false;
  }
}*/

export function showStatus(status: Status | Transform): string {
  switch (status.tag) {
    // Status
    case "Poison": {
      return `Poison ${status.value} T ${status.fragment} F`;
    }
    case "PiercingPoison": {
      return `PiercingPoison ${status.value} T ${status.fragment} F`;
    }
    case "Regen": {
      return `Regen ${status.value} T ${status.fragment} F`;
    }
    case "Doom": {
      return `Doom ${status.value} T ${status.fragment} F`;
    }
    case "Blind": {
      return `Blind ${status.value} T ${status.fragment} F`;
    }
    case "Silence": {
      return `Silence ${status.value} T ${status.fragment} F`;
    }
    case "DmgBarrier": {
      return `DmgBarrier ${status.value} T ${status.fragment} F`;
    }
    case "Mark": {
      return `Mark ${status.value} T ${status.fragment} F`;
    }
    case "Intercept": {
      return `Intercept ${status.value} T ${status.fragment} F`;
    }
    // Transform
    case "Guard": {
      return `Guard ${status.guard} ${status.value} T ${status.fragment} F`;
    }
    case "Bubble": {
      return `Bubble`;
    }
    case "Weak": {
      return `Weak ${status.value} T ${status.fragment} F`;
    }
    case "Strong": {
      return `Strong ${status.value} T ${status.fragment} F`;
    }
    case "Convert": {
      return `Convert ${status.value} T ${status.fragment} F`;
    }
  }
}

type DiscrStatus<T extends Status["tag"]> = Extract<Status, {tag: T}>

export type HasStatus = {
  status: Status[],
  fragmentLoss: { [key in StatusTag]?: number }
};

export type HasTransform = {
  transforms: Transform[],
  fragmentLoss: { [key in TransformTag]?: number }
};

function mergeStatus<S extends Status | Transform>(
  status1: S,
  status2: S,
): S {
  switch (status1.tag) {
    case "Poison": {
      return collapseStatus(focus(status1,
        over(x => x.fragment, x => x + status2.fragment),
        over(x => x.value, x => x + status2.value),
      ));
    }
    case "Weak": {
      return collapseStatus(focus(status1,
        over(x => x.fragment, x => x + status2.fragment),
        over(x => x.value, x => x + status2.value),
      ));
    }
    case "Strong": {
      return collapseStatus(focus(status1,
        over(x => x.fragment, x => x + status2.fragment),
        over(x => x.value, x => x + status2.value),
      ));
    }
    case "Mark": {
      return collapseStatus(focus(status1,
        over(x => x.fragment, x => x + status2.fragment),
        over(x => x.value, x => x + status2.value),
      ));
    }
    default: {
      throw "mergeStatus: TODO";
    }
  }
}

function collapseStatus<S extends Status | Transform>(
  status: S,
): S {
  const fragment = status.fragment;
  const newFragment = fragment % 100;
  const addValue = Math.floor(fragment / 100);
  return focus(status,
    set(x => x.fragment, newFragment),
    over(x => x.value, x => x + addValue),
  );
}

export function removeStatus<E extends HasStatus>(
  e: E,
  tag: StatusTag,
) {
  return focus(e,
    over(x => x.status, x => x.filter(x => x.tag !== tag)),
  );
}

export function addStatus<E extends HasStatus>(
  e: E,
  status: Status,
): E {
  // if (status.tag)
  // else 
  // merge statuses
  if (status.tag === "DmgBarrier") {
    return focus(e,
      over(x => x.status, x => x.concat(status)),
    );
  } else {
    // merge statuses
    const filtered = e.status
      .map((x, i) => { return { x, i }} )
      .filter(x => x.x.tag === status.tag);
    if (filtered.length > 1) {
      throw "addStatus: length>1 should be impossible";
    } else if (filtered.length === 1) {
      const { x, i } = filtered[0];
      return focus(e,
        over(x => x.status[i], x => mergeStatus(x, status)),
      );
    } else {
      return focus(e,
        over(x => x.status, x => x.concat(status)),
      );
    }
  }
}

export function addTransform<E extends HasTransform>(
  e: E,
  transform: Transform,
): E {
  // if (status.tag)
  // else 
  // merge statuses
  if (transform.tag === "Bubble") {
    return focus(e,
      over(x => x.transforms, x => x.concat(transform)),
    );
  } else {
    // merge statuses
    const filtered = e.transforms
      .map((x, i) => { return { x, i }} )
      .filter(x => x.x.tag === transform.tag);
    if (filtered.length > 1) {
      throw "addStatus: length>1 should be impossible";
    } else if (filtered.length === 1) {
      const { x, i } = filtered[0];
      return focus(e,
        over(x => x.transforms[i], x => mergeStatus(x, transform)),
      );
    } else {
      return focus(e,
        over(x => x.transforms, x => x.concat(transform)),
      );
    }
  }
}

export function addStatusTransform<E extends HasStatus & HasTransform>(
  e: E,
  statusTransform: Status | Transform,
) {
  if (
    statusTransform.tag === "Poison" ||
    statusTransform.tag === "PiercingPoison" ||
    statusTransform.tag === "Regen" ||
    statusTransform.tag === "Doom" ||
    statusTransform.tag === "Blind" ||
    statusTransform.tag === "Silence" ||
    statusTransform.tag === "DmgBarrier" ||
    statusTransform.tag === "Mark" ||
    statusTransform.tag === "Intercept"
  ) {
    return addStatus(e, statusTransform);
  } else {
    return addTransform(e, statusTransform);
  }
}

export function loseFragments<E extends HasStatus & Id>(
  e: E,
  tag: StatusTag | TransformTag,
  loss: number,
): E {
  const filtered = e.status
    .map((x, i) => { return { x, i}} )
    .filter(x => x.x.tag === tag);
  for (const { x, i } of filtered) {
    const newStatus = applyStatusLoss(x, loss);
    e = focus(e,
      set(x => x.status[i], newStatus),
    );
  }
  return focus(e,
    over(x => x.status, x => x.filter(x => x !== undefined)),
  );
}

export function applyLoseFragmentPhase(
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ApplyActionLog } {
  let i = 0;
  for (const _enemy of state.enemies) {
    const afterStatus = applyLoseFragments(
      { tag: "PositionId", type: "enemy", id: i }, state, log
    );
    if (afterStatus.state === "invalid") {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    i += 1;
  }
  i = 0;
  for (const _ally of state.crew) {
    const afterStatus = applyLoseFragments(
      { tag: "PositionId", type: "ally", id: i }, state, log
    );
    if (afterStatus.state === "invalid") {
      return afterStatus;
    }
    state = afterStatus.state;
    log = afterStatus.log;
    i += 1;
  }
  return { state, log };
}

function applyStatusLoss(
  status: Status,
  loss: number,
): Status | undefined {
  const fragmentValue = 100 * status.value + status.fragment;
  if (fragmentValue > loss) {
    const newFragmentValue = fragmentValue - loss;
    const newFragment = newFragmentValue % 100;
    const newStatus = Math.floor(newFragmentValue / 100);
    return focus(status,
      set(x => x.value, newStatus),
      set(x => x.fragment, newFragment),
    );
  } else {
    return undefined;
  }
}

export function applyLoseFragments(
  id: CreatureId,
  state: GameState,
  log: ApplyActionLog,
): { state: GameState | "invalid", log: ApplyActionLog } {
  const e = findEntity(state, id);
  const newStatusList: Status[] = [];
  for (const status of e.status) {
    const loss = e.fragmentLoss[status.tag] === undefined ? 0 : <number>e.fragmentLoss[status.tag];
    const newStatus = applyStatusLoss(status, loss);
    if (newStatus !== undefined) {
      newStatusList.push(newStatus);
    }
  }
  const pos = toPositionId(state, id).id;
  if (id.type === "ally") {
    state = focus(state,
      set(x => x.crew[pos].status, newStatusList),
    );
  } else if (id.type === "enemy") {
    state = focus(state,
      set(x => x.enemies[pos].status, newStatusList),
    );
  }
  return { state, log };
}

export function checkTransforms<E extends HasTransform & { charges: number }>(
  trigger: Action,
  e: E,
  state: GameState,
  selfId: CreatureId,
  log: ApplyActionLog,
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ApplyActionLog, action: Action } {
  let action = trigger;
  for (const transform of e.transforms) {
    const triggerEff = transformToTrigger(transform.tag);

    const effect = triggerEff.effect({ state, selfId, trigger: action, transform, triggerOrigin: origin });
    if (effect.action.tag !== "Noop" && effect.chargeUse <= e.charges) {
      action = effect.action;
      const result = applyActionAndTriggers({
        tag: "ChargeUse", target: selfId, value: effect.chargeUse
      }, state, log, idGen, origin);
      const extraLine: ApplyActionLine = { tag: "TransformAction", transformTo: action, log: result.log };
      log = log.concat(extraLine);
      if (result.state === "invalid") {
        return { state: "invalid", log, action };
      }
      state = result.state;
    }
  }
  return { state, log, action };
}

// TODO: implement difference between before/instead/after triggers
export function checkStatus<E extends HasStatus & { charges: number }>(
  trigger: Action,
  e: E,
  state: GameState,
  selfId: CreatureId,
  log: ApplyActionLog,
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: ApplyActionLog, abort: boolean } {
  for (const status of e.status) {
    const triggerEff = statusToTrigger(status.tag);
    const effect = triggerEff.effect({ state, selfId, trigger, status, triggerOrigin: origin });
    if (effect.action.tag === "Noop" && effect.chargeUse === 0) {
      // skip noop/0 charge to prevent infinite loop
    } else if (effect.action.tag === "Abort" && effect.chargeUse === 0) {
      return { state, log, abort: true };
    } else if (effect.chargeUse <= e.charges) {
      const result = applyActionAndTriggers({
        tag: "CombinedAction",
        actions: [
          { tag: "ChargeUse", target: selfId, value: effect.chargeUse },
          effect.action
        ]
      }, state, log, idGen, origin);
      const extraLine: ApplyActionLine = { tag: "TriggerBeforeAction", status, log: result.log };
      log = log.concat(extraLine);
      if (result.state === "invalid") {
        return { state: "invalid", log, abort: true };
      }
      state = result.state;
    }
  }
  return { state, log, abort: false };
}

export function statusToTrigger(
  tag: StatusTag,
): TriggerEntityEffect {
  switch (tag) {
    case "Poison": {
      return evTrigger(trigger => evCondition(trigger,
          x => x.tag === "StartTurn",
          extra(damage(evSelf, evStatusValue, evStatic(false)), { chargeUse: 0 }),
          extra(noop(), { chargeUse: 0 }),
        ),
      );
    }
    case "DmgBarrier": {
      return dmgBarrierTrigger;
    }
    case "Mark": {
      return evTrigger(trigger => evCondition(trigger,
          x => x.tag === "StartTurn",
          extra(EV.loseFragments(evSelf, evStatic(<"Mark">"Mark"), evStatic(-50)), { chargeUse: 0 }),
          extra(noop(), { chargeUse: 0 }),
        ),
      );
    }
    case "Intercept": {
      return EV.interceptTrigger;
    }
    default: throw "unimplemented";
  }
}

export function transformToTrigger(
  tag: TransformTag,
): TriggerEntityEffect {
  switch (tag) {
    case "Guard": {
      return EV.guardTrigger;
    }
    case "Weak": {
      return EV.weakTrigger;
    }
    case "Convert": {
      return EV.convertTrigger;
    }
    case "Bubble": {
      return EV.bubbleTrigger;
    }
    case "Strong": {
      return EV.strongTrigger;
    }
  }
}

// should be used to find mergeable status
// throws error when more than 1 status is found
export function findStatus<E extends HasStatus>(
  e: E,
  tag: StatusTag,
): Status | undefined {
  const filtered = e.status.filter(x => x.tag === tag);
  if (filtered.length > 1) {
    throw `findStatus: unexpected more than 1 status of ${tag}`;
  } else if (filtered.length === 1) {
    return filtered[0];
  } else {
    return undefined;
  }
}