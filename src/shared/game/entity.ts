import { focus, over } from "../iassign-util";

type Entity = {
  hp: number,
}

export function damageEntity<E extends Entity>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.hp, x => x - value),
  );
}

