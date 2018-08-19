import { focus, over, set } from "src/shared/iassign-util";
import { ActionSpec, Action } from "src/shared/game/action";
import { TargetType, typeColl } from "src/shared/game/target";
import { findIndex } from "./trigger";
import { GameState } from "./state";

export type Poison = {
  tag: "Poison",
  value: number,
};

export type Regen = {
  tag: "Regen",
  value: number,
};

export type Guard = {
  tag: "Guard",
  value: number,
  guard: number,
};

export type Doom = {
  tag: "Doom",
  value: number,
};

export type Blind = {
  tag: "Blind",
  value: number,
};

export type Silence = {
  tag: "Silence",
  value: number,
};

export type Status
  = Poison
  | Regen
  | Guard
  | Doom
  | Blind
  | Silence
  ;

export function showStatus(status: Status): string {
  switch (status.tag) {
    case "Poison": {
      return `Poison ${status.value} T`;
    }
    case "Regen": {
      return `Regen ${status.value} T`;
    }
    case "Guard": {
      return `Guard ${status.guard} ${status.value} T`;
    }
    case "Doom": {
      return `Doom ${status.value} T`;
    }
    case "Blind": {
      return `Blind ${status.value} T`;
    }
    case "Silence": {
      return `Silence ${status.value} T`;
    }
  }
}

export const allStatus: Status["tag"][] = ["Regen", "Poison", "Doom", "Guard", "Blind", "Silence"];

// conditional types
type StatusType<A> =
  A extends "Poison" ? Poison :
  A extends "Regen" ? Regen :
  A extends "Guard" ? Guard :
  A extends "Doom" ? Doom :
  A extends "Blind" ? Blind :
  A extends "Silence" ? Silence :
  never;

export type HasStatus = {
  // lookup types
  [key in Status["tag"]]?: StatusType<key>
};

export function addStatus<E extends HasStatus>(
  e: E,
  status: Status,
): E {
  if (e[status.tag] === undefined) {
    return focus(e, set(x => x[status.tag], status));
  } else {
    // cast to prevent 'undefined' warning
    return focus(e, over(x => (<Status>x[status.tag]).value, x => x + status.value));
  }
}

export function clearStatus<E extends HasStatus>(
  e: E,
  statusTag: Status["tag"],
) {
  return focus(e, set(x => x[statusTag], undefined));
}

export function applyStatus<E extends HasStatus>(
  _index: number,
  e: E,
  tag: Status["tag"],
): E {
  const status = e[tag];
  if (status !== undefined) {
    if (status.value === 1) {
      e = focus(e,
        set(x => x[tag], undefined),
      );
    } else {
      e = focus(e,
        over(x => x[tag]!.value, x => x - 1),
      );
    }
  }
  return e;
}

export function statusToAction(
  status: Status,
  state: GameState,
  selfId: number,
  selfType: TargetType,
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
          tag: "Target",
          positions: [selfIndex],
          type: selfType,
        },
        value: status.value,
      };
    }
    case "Regen": {
      return {
        tag: "Heal",
        target: {
          tag: "Target",
          positions: [selfIndex],
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
  }
}