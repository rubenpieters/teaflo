import { focus, over, set } from "src/shared/iassign-util";
import { ActionTarget, ActionSpec } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { TargetType } from "src/shared/game/target";

export type Poison = {
  tag: "Poison",
  value: number,
};

export type Regen = {
  tag: "Regen",
  value: number,
};

export type Status
  = Poison
  | Regen
  ;

export type HasStatus = {
  status: Status[],
};

export function addStatus<E extends HasStatus>(
  e: E,
  status: Status,
): E {
  const index = findIndex(e.status, s => s.tag === status.tag);
  if (index === "notFound") {
    return focus(e,
      over(x => x.status, x => x.concat(status)),
    );
  } else {
    return focus(e,
      over(x => x.status[index].value, x => x + status.value),
    );
  }
}

function findIndex<A>(
  as: A[],
  predicate: (a: A) => boolean,
): number | "notFound" {
  let index: number = 0;
  for (const a of as) {
    if (predicate(a)) {
      return index;
    }
    index += 1;
  }
  return "notFound";
}

export function applyStatus<E extends HasStatus>(
  index: number,
  e: E,
): E {
  if (e.status[index].value === 1) {
    e = focus(e,
      over(x => x.status, x => x.slice(0, index).concat(x.slice(index + 1, x.length - 1))),
    );
  } else {
    e = focus(e,
      over(x => x.status[index].value, x => x - 1),
    );
  }
  return e;
}

export function statusToAction(
  status: Status,
): ActionSpec {
  switch (status.tag) {
    case "Poison": {
      return {
        tag: "Damage",
        target: {
          tag: "Self",
        },
        value: status.value,
      };
    }
    case "Regen": {
      throw "unimplemented";
    }
  }
}