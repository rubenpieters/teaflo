import { focus, over, set } from "src/shared/iassign-util";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Origin, TargetType, typeColl } from "src/shared/game/target";
import { findIndex } from "src/shared/game/trigger";
import { GameState, CreatureId, toPositionId, Id, findEntity } from "src/shared/game/state";
import { evStatic, evAnd, evAllies, evSelf, damage, addTarget, queueStatus, noTarget, chargeUse, heal, noop, evCondition, evTrigger, extra, addThreat, evEnemies, evStatusValue, guardTrigger } from "src/shared/game/effectvar";
import { TriggerEntityEffect } from "src/shared/game/ability";
import { Generator } from "src/shared/handler/id/generator";

export type StatusTag = Status["tag"];

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

export type Status
  = Poison
  | PiercingPoison
  | Regen
  | Guard
  | Doom
  | Blind
  | Silence
  | Bubble
  ;

export function showStatus(status: Status): string {
  switch (status.tag) {
    case "Poison": {
      return `Poison ${status.value} T ${status.fragment} F`;
    }
    case "PiercingPoison": {
      return `PiercingPoison ${status.value} T ${status.fragment} F`;
    }
    case "Regen": {
      return `Regen ${status.value} T ${status.fragment} F`;
    }
    case "Guard": {
      return `Guard ${status.guard} ${status.value} T ${status.fragment} F`;
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
    case "Bubble": {
      return `Bubble`;
    }
  }
}

export const allStatus: Status["tag"][] = ["Regen", "PiercingPoison", "Poison", "Doom", "Guard", "Blind", "Silence", "Bubble"];

type DiscrStatus<T extends Status["tag"]> = Extract<Status, {tag: T}>

export type HasStatus = {
  status: Status[],
  fragmentLoss: { [key in Status["tag"]]?: number }
};

function mergeStatus<S extends Status>(
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
    default: {
      throw "TODO";
    }
  }
}

function collapseStatus<S extends Status>(
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

export function addStatus<E extends HasStatus>(
  e: E,
  status: Status,
): E {
  // if (status.tag)
  // else 
  // merge statuses
  const filtered = e.status
    .map((x, i) => { return { x, i}} )
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

  /*let result: E;
  if (e.status[status.tag] === undefined) {
    result = focus(e, set(x => x.status[status.tag], status));
  } else if (status.tag === "Guard") {
    // cast to prevent 'undefined' warning
    result = focus(e,
      over(x => (<Guard>x.status[status.tag]).value, x => x + status.value),
      over(x => (<Guard>x.status[status.tag]).fragment, x => x + status.fragment),
      over(x => (<Guard>x.status[status.tag]).guard, x => x + status.guard),
    );
  } else if (status.tag === "Bubble") {
    result = e;
  } else {
    // cast to prevent 'undefined' warning
    result = focus(e,
      over(x => (<Status>x.status[status.tag]).value, x => x + status.value),
      over(x => (<Status>x.status[status.tag]).fragment, x => x + status.fragment),
    );
  }

  if (result.status[status.tag]!.fragment > 99) {
    const toAdd = Math.floor(result.status[status.tag]!.fragment / 100);
    result = focus(result,
      over(x => x.status[status.tag]!.value, x => x + toAdd),
      over(x => x.status[status.tag]!.fragment, x => x - toAdd * 100),
    );
  }

  return result;*/
}

export function loseFragments<E extends HasStatus & Id>(
  e: E,
  tag: StatusTag,
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
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  let i = 0;
  for (const _enemy of state.enemies) {
    const afterStatus = applyLoseFragments(
      { tag: "PositionId", type: "enemy", id: i }, state, log, idGen, origin
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
      { tag: "PositionId", type: "ally", id: i }, state, log, idGen, origin
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

export function applyLoseFragments<E extends HasStatus & Id>(
  id: CreatureId,
  state: GameState,
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  const e = findEntity(state, id);
  const newStatusList: Status[] = [];
  for (const status of e.status) {
    const loss = e.fragmentLoss[status.tag] === undefined ? 0 : <number>e.fragmentLoss[status.tag];
    console.log(`LOSS: ${loss}`);
    const newStatus = applyStatusLoss(status, loss);
    if (newStatus !== undefined) {
      newStatusList.push(newStatus);
    }
  }
  const pos = toPositionId(state, id).id;
  state = focus(state,
    set(x => x.crew[pos].status, newStatusList),
  );
  return { state, log };
  /*for (const tag of allStatus) {
    const e = findEntity(state, id);
    if (e.status[tag] !== undefined) {
      const loss = e.fragmentLoss[tag] === undefined ? 0 : <number>e.fragmentLoss[tag];
      const result = applyActionAndTriggers({ tag: "LoseFragment", target: id, type: tag, value: loss }, state, log, idGen, origin);
      if (result.state === "invalid") {
        return { state: "invalid", log: result.log };
      }
      state = result.state;
      log = result.log;
    }
  }
  return { state, log };*/
}

/*export function clearStatus<E extends HasStatus>(
  e: E,
  statusTag: Status["tag"],
) {
  return focus(e, set(x => x.status[statusTag], undefined));
}*/

// TODO: implement difference between before/instead/after triggers
export function checkStatus<E extends HasStatus & { charges: number }>(
  trigger: Action,
  e: E,
  state: GameState,
  selfId: CreatureId,
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[], abort: boolean } {
  for (const status of e.status) {
    const triggerEff = statusToTrigger(status.tag);
    const effect = triggerEff.effect({ state, selfId, trigger, status, triggerOrigin: origin });
    if (effect.action.tag === "Noop" && effect.chargeUse === 0) {
      // skip noop/0 charge to prevent infinite loop
    } else if (effect.chargeUse <= e.charges) {
      const result = applyActionAndTriggers({
        tag: "CombinedAction",
        actions: [
          { tag: "ChargeUse", target: selfId, value: effect.chargeUse },
          effect.action
        ]
      }, state, log, idGen, origin);
      if (result.state === "invalid") {
        return { state: "invalid", log: result.log, abort: true };
      }
      state = result.state;
      log = result.log;
      if (triggerEff.type === "instead") {
        return { state, log, abort: true };
      }
    }
  }
  return { state, log, abort: false };
}

function statusToTrigger(
  tag: Status["tag"],
): TriggerEntityEffect {
  switch (tag) {
    case "Poison": {
      return evTrigger(trigger => evCondition(trigger,
          x => x.tag === "StartTurn",
          extra(damage(evSelf, evStatusValue, evStatic(false)), { chargeUse: 0 }),
          extra(noop(), { chargeUse: 0 }),
        ),
        "before",
      );
    }
    case "Guard": {
      return guardTrigger;
    }
    default: throw "unimplemented";
  }
}

export function statusToAction(
  status: Status,
  state: GameState,
  selfId: number,
  selfType: "ally" | "enemy",
): Action {
  const selfIndex = findIndex(x => x.id === selfId, typeColl(state, selfType));
  if (selfIndex === "notFound") {
    throw `not found ${selfId} ${selfType}`
  }
  switch (status.tag) {
    case "Poison": {
      return {
        tag: "Damage",
        target: {
          tag: "PositionId",
          id: selfIndex,
          type: selfType,
        },
        value: status.value,
        piercing: false,
      };
    }
    case "PiercingPoison": {
      return {
        tag: "Damage",
        target: {
          tag: "PositionId",
          id: selfIndex,
          type: selfType,
        },
        value: status.value,
        piercing: true,
      };
    }
    case "Regen": {
      return {
        tag: "Heal",
        target: {
          tag: "PositionId",
          id: selfIndex,
          type: selfType,
        },
        value: status.value,
      };
    }
    case "Guard": {
      return { tag: "Noop" };
    }
    case "Doom": {
      if (status.value === 1) {
        return {
          tag: "Death",
          id: selfId,
          type: selfType,
        };
      } else {
        return { tag: "Noop" };
      }
    }
    case "Blind": {
      return { tag: "Noop" };
    }
    case "Silence": {
      return { tag: "Noop" };
    }
    case "Bubble": {
      return { tag: "Noop" };
    }
  }
}