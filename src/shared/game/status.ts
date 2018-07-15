import { focus, over, set } from "src/shared/iassign-util";

export type Poison = {
  tag: "Poison",
  value: number,
};

export type Regen = {
  tag: "Poison",
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