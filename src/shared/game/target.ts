import { focus, over, set } from "src/shared/iassign-util";
import { GameState, Id, IdCrew, IdEnemy, IdItem } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";

export type Origin = {
  id: number,
  type: TargetType,
} | "noOrigin";

export type Self = {
  tag: "Self",
};

export type All = {
  tag: "All",
  type: TargetType,
};

export type Last = {
  tag: "Last",
  type: TargetType,
};

export type Positions = {
  tag: "Positions",
  type: TargetType,
  positions: number[],
};

export type OriginTarget = {
  tag: "OriginTarget",
};

export type TargetSpec
  = Self
  | All
  | Last
  | Positions
  | OriginTarget
  ;

export function determineTarget(
  target: TargetSpec,
  state: GameState,
  selfId: number,
  selfType: TargetType,
  origin: Origin,
): Target {
  switch (target.tag) {
    case "Self": {
      const coll = typeColl(state, selfType);
      const index = indexOfId(selfId, coll);
      if (index === "notFound") {
        throw ("index " + index + " not found");
      } else {
        return {
          tag: "Target",
          type: selfType,
          positions: [index],
        };
      }
    }
    case "All": {
      if (target.type === "ally") {
        // This means we always have to take into account non-existing positions
        return {
          tag: "Target",
          type: target.type,
          positions: [...Array(state.crewLimit).keys()],
        };
      } else {
        const coll = typeColl(state, target.type);
        return {
          tag: "Target",
          type: target.type,
          positions: coll.map((v, i) => i),
        };
      }
    }
    case "Last": {
      const coll = typeColl(state, selfType);
      return {
        tag: "Target",
        type: target.type,
        positions: [coll.length - 1],
      };
    }
    case "Positions": {
      return {
        tag: "Target",
        type: target.type,
        positions: target.positions,
      };
    }
    case "OriginTarget": {
      if (origin === "noOrigin") {
        throw "No origin!";
      } else {
        return {
          tag: "Target",
          type: origin.type,
          positions: [origin.id],
        };
      }
    }
  }
}

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
  positions: number[],
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
        over(x => x.crew, x => x.map((a, i) => {
          if (target.positions.indexOf(i) === -1) {
            return a;
          } else {
            return allyF(a);
          }
        })),
      );
    }
    case "enemy": {
      return focus(state,
        over(x => x.enemies, x => x.map((a, i) => {
          if (target.positions.indexOf(i) === -1) {
            return a;
          } else {
            return enemyF(a);
          }
        })),
      );
    }
    case "item": {
      return focus(state,
        over(x => x.items, x => x.map((a, i) => {
          if (target.positions.indexOf(i) === -1) {
            return a;
          } else {
            return itemF(a);
          }
        })),
      );
    }
  }
}

export function showTarget(target: Target | TargetSpec): string {
  switch (target.tag) {
    case "Target": {
      return target.positions + " " + target.type;
    }
    case "Self": {
      return "self";
    }
    case "All": {
      return "all " + target.type;
    }
    case "Last": {
      return "last " + target.type;
    }
    case "Positions": {
      return target.positions + " " + target.type;
    }
    case "OriginTarget": {
      return "origin";
    }
  }
}
