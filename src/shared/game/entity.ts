import { focus, over } from "../iassign-util";

type Entity = {
  hp: number,
  maxHp: number,
}

export function damageEntity<E extends Entity>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.hp, x => x - value),
  );
}

export function healEntity<E extends Entity>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.hp, x => Math.min(e.maxHp, x + value)),
  );
}