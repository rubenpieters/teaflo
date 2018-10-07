import { focus, over, set } from "src/shared/iassign-util";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Origin, TargetType, typeColl } from "src/shared/game/target";
import { findIndex } from "src/shared/game/trigger";
import { GameState, CreatureId, toPositionId, Id, findEntity } from "src/shared/game/state";
import { evStatic, evAnd, evAllies, evSelf, damage, addTarget, queueStatus, noTarget, chargeUse, heal, noop, evCondition, evTrigger, extra, addThreat, evEnemies } from "src/shared/game/effectvar";
import { TriggerEntityEffect } from "src/shared/game/ability";
import { Generator } from "src/shared/handler/id/generator";

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
  status: { [key in Status["tag"]]?: DiscrStatus<key> }
  fragmentLoss: { [key in Status["tag"]]?: number }
};

export function addStatus<E extends HasStatus>(
  e: E,
  status: Status,
): E {
  let result: E;
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

  return result;
}

export function loseFragments<E extends HasStatus & Id>(
  e: E,
  tag: Status["tag"],
): E {
  const status = e.status[tag];
  if (status !== undefined) {
    const loss = e.fragmentLoss[tag] === undefined ? 0 : <number>e.fragmentLoss[tag];
    const fragmentValue = 100 * status.value + status.fragment;
    if (fragmentValue > loss) {
      const newFragmentValue = fragmentValue - loss;
      const newFragment = newFragmentValue % 100;
      const newStatus = Math.floor(newFragmentValue / 100);
      e = focus(e,
        set(x => x.status[tag]!.value, newStatus),
        set(x => x.status[tag]!.fragment, newFragment),
      );
    } else {
      e = focus(e,
        set(x => x.status[tag], undefined),
      );
    }
  }
  return e;
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

export function applyLoseFragments<E extends HasStatus & Id>(
  id: CreatureId,
  state: GameState,
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  for (const tag of allStatus) {
    const e = findEntity(state, id);
    if (e.status[tag] !== undefined) {
      const result = applyActionAndTriggers({ tag: "LoseFragment", target: id, type: tag }, state, log, idGen, origin);
      if (result.state === "invalid") {
        return { state: "invalid", log: result.log };
      }
      state = result.state;
      log = result.log;
    }
  }
  return { state, log };
}

export function clearStatus<E extends HasStatus>(
  e: E,
  statusTag: Status["tag"],
) {
  return focus(e, set(x => x.status[statusTag], undefined));
}

export function applyStatus<E extends HasStatus>(
  _index: number,
  e: E,
  tag: Status["tag"],
): E {
  const status = e.status[tag];
  if (status !== undefined) {
    if (status.value === 1 && status.fragment === 0) {
      e = focus(e,
        set(x => x.status[tag], undefined),
      );
    } else if (status.value <= 1) {
      e = focus(e,
        set(x => x.status[tag]!.value, 0),
      );
    } else {
      e = focus(e,
        over(x => x.status[tag]!.value, x => x - 1),
      );
    }
  }
  return e;
}

// TODO: implement difference between before/instead/after triggers
export function checkStatus<E extends HasStatus & { charges: number }>(
  trigger: Action,
  e: E,
  state: GameState,
  selfId: CreatureId,
  log: Action[],
  idGen: Generator,
  origin: Origin,
): { state: GameState | "invalid", log: Action[] } {
  for (const tag of allStatus) {
    const status = e.status[tag];
    if (status !== undefined) {
      const effect = statusToTrigger(status).effect({ state, selfId, trigger });
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
          return { state: "invalid", log: result.log };
        }
        state = result.state;
        log = result.log;
      }
    }
  }
  return { state, log };
}

function statusToTrigger(
  status: Status,
): TriggerEntityEffect {
  switch (status.tag) {
    case "Poison": {
      return evTrigger(trigger => evCondition(trigger,
          x => x.tag === "StartTurn",
          extra(damage(evSelf, evStatic(status.value), evStatic(false)), { chargeUse: 0 }),
          extra(noop(), { chargeUse: 0 }),
        ),
        "before",
      );
    }
    case "Guard": {
      return evTrigger(trigger => evCondition(trigger,
          x => x.tag === "Damage", // and target is self
          extra(noop(), { chargeUse: 0 }),
          extra(noop(), { chargeUse: 0 }),
        ),
        "before",
      );
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