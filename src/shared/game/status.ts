import { focus, over, set } from "src/shared/iassign-util";
import { ActionSpec, Action } from "src/shared/game/action";
import { TargetType, typeColl } from "src/shared/game/target";
import { findIndex } from "./trigger";
import { GameState } from "./state";

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

export type Status
  = Poison
  | PiercingPoison
  | Regen
  | Guard
  | Doom
  | Blind
  | Silence
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
  }
}

export const allStatus: Status["tag"][] = ["Regen", "PiercingPoison", "Poison", "Doom", "Guard", "Blind", "Silence"];

// conditional types
type StatusType<A> =
  A extends "Poison" ? Poison :
  A extends "PiercingPoison" ? Poison :
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
  let result: E;
  if (e[status.tag] === undefined) {
    result = focus(e, set(x => x[status.tag], status));
  } else if (status.tag === "Guard") {
    // cast to prevent 'undefined' warning
    result = focus(e,
      over(x => (<Guard>x[status.tag]).value, x => x + status.value),
      over(x => (<Guard>x[status.tag]).fragment, x => x + status.fragment),
      over(x => (<Guard>x[status.tag]).guard, x => x + status.guard),
    );
  } else {
    // cast to prevent 'undefined' warning
    result = focus(e,
      over(x => (<Status>x[status.tag]).value, x => x + status.value),
      over(x => (<Status>x[status.tag]).fragment, x => x + status.fragment),
    );
  }

  if (result[status.tag]!.fragment > 99) {
    const toAdd = Math.floor(result[status.tag]!.fragment / 100);
    result = focus(result,
      over(x => x[status.tag]!.value, x => x + toAdd),
      over(x => x[status.tag]!.fragment, x => x - toAdd * 100),
    );
  }

  return result;
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
    if (status.value === 1 && status.fragment === 0) {
      e = focus(e,
        set(x => x[tag], undefined),
      );
    } else if (status.value <= 1) {
      e = focus(e,
        set(x => x[tag]!.value, 0),
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
          position: selfIndex,
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
          tag: "Target",
          position: selfIndex,
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
          tag: "Target",
          position: selfIndex,
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