import { focus, over, set } from "src/shared/iassign-util";
import { Action, applyActionAndTriggers } from "src/shared/game/action";
import { Origin, TargetType, typeColl } from "src/shared/game/target";
import { findIndex } from "src/shared/game/trigger";
import { GameState, CreatureId } from "src/shared/game/state";
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

export function checkStatus<E extends HasStatus & { charges: number }>(
  trigger: Action,
  e: E,
  state: GameState,
  selfId: CreatureId,
  log: Action[],
  idGen: Generator,
  origin: Origin,
) {
  for (const tag of allStatus) {
    const status = e.status[tag]
    if (status !== undefined) {
      const effect = statusToTrigger(status).effect({ state, selfId, trigger });
      if (effect.chargeUse > e.charges) {
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