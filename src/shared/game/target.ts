import { focus, over, set } from "src/shared/iassign-util";
import { GameState, Id, IdCrew, IdEnemy, IdItem } from "src/shared/game/state";

export type Origin = {
  id: number,
  type: TargetType,
} | "noOrigin";

export function typeColl(
  state: GameState,
  type: TargetType
): (IdCrew | IdEnemy | IdItem)[] {
  switch (type) {
    case "ally": {
      return state.crew;
    }
    case "enemy": {
      return state.enemies;
    }
    case "item": {
      return state.items;
    }
  }
}

export function indexOfId<E extends Id>(
  id: number,
  es: E[],
): number | "notFound" {
  return findIndex(e => e.id === id, es);
}

function findIndex<A>(
  predicate: (a: A) => boolean,
  as: A[],
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

export type Target = {
  tag: "Target",
  type: TargetType;
  position: number,
};

export type TargetType = "ally" | "enemy" | "item";

export function onTarget(
  target: Target,
  state: GameState,
  allyF: (ally: IdCrew) => IdCrew,
  enemyF: (enemy: IdEnemy) => IdEnemy,
  itemF: (item: IdItem) => IdItem,
) {
  switch (target.type) {
    case "ally": {
      return focus(state,
        over(x => x.crew[target.position], allyF),
      );
    }
    case "enemy": {
      return focus(state,
        over(x => x.enemies[target.position], enemyF),
      );
    }
    case "item": {
      return focus(state,
        over(x => x.items[target.position], itemF),
      );
    }
  }
}

export function showTarget(target: Target): string {
  switch (target.tag) {
    case "Target": {
      return `${target.position} ${target.type}`;
    }
  }
}
