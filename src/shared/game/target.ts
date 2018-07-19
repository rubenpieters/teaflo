import { focus, over, set } from "src/shared/iassign-util";
import { GameState, Id, IdCrew, IdEnemy, IdItem } from "src/shared/game/state";
import { Enemy } from "src/shared/game/enemy";

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

export type TargetSpec
  = Self
  | All
  | Last
  | Positions
  ;

export function determineTarget(
  target: TargetSpec,
  state: GameState,
  selfId: number,
  selfType: TargetType
): Target {
  const coll = typeColl(state, selfType);
  switch (target.tag) {
    case "Self": {
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
      return {
        tag: "Target",
        type: target.type,
        positions: coll.map((v, i) => i),
      };
    }
    case "Last": {
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
  }
}

/*
export type Self = {
  tag: "Self",
};

export type AllCrewSpec = {
  tag: "AllCrew",
};

export type TargetEnemySpec = {
  tag: "TargetEnemy",
};

export type LastCrew = {
  tag: "LastCrew",
};

export type TargetSpec
  = Self
  | AllCrewSpec
  | TargetEnemySpec
  | LastCrew
  ;

export type Positions = {
  tag: "Positions",
  type: TargetType
  positions: number[],
};

export type TargetId = {
  tag: "TargetId",
  type: TargetType,
  id: number,
};

export type AllCrew = {
  tag: "AllCrew",
  type: "ally",
};

export type TargetEnemy = {
  tag: "TargetEnemy",
  type: "enemy",
};




export function findTarget(
  targetSpec: TargetSpec,
  state: GameState,
  selfId: number,
  selfType: TargetType,
): Target {
  switch (targetSpec.tag) {
    case "Self": {
      return {
        tag: "TargetId",
        type: selfType,
        id: selfId,
      };
    }
    case "AllCrew": {
      return {...targetSpec, ...{ type: "ally" } };
    }
    case "LastCrew": {
      if (state.crew.length > 0) {
        return {
          tag: "TargetId",
          type: "ally",
          id: state.crew[state.crew.length - 1].id
        };
      } else {
        // TODO: add target wiffing
        throw "no crew while trying to target last crew";
      }
    }
    case "TargetEnemy": {
      return {...targetSpec, ...{ type: "enemy" } };
    }
  }
}

export function onCrew(
  target: Target,
  state: GameState,
  modify: (e: IdCrew) => IdCrew
) {
  if (target.type === "ally") {
    return onTargets(target, state, gs => gs.crew, modify);
  } else {
    throw ("wrong target type: " + target.type + ", expected 'ally'");
  }
}

export function onItem(
  target: Target,
  state: GameState,
  modify: (e: IdItem) => IdItem
) {
  if (target.type === "item") {
    return onTargets(target, state, gs => gs.items, modify);
  } else {
    throw ("wrong target type: " + target.type + ", expected 'item'");
  }
}

export function onTargets<E extends Id>(
  target: Target,
  state: GameState,
  get: (gs: GameState) => E[],
  modify: (e: E) => E,
): GameState {
  switch (target.tag) {
    case "TargetId": {
      const targetIndex = indexOfId(target.id, get(state));
      if (targetIndex === "notFound") {
        return state;
      } else {
        return focus(state,
          over(x => get(x)[targetIndex], modify),
        );
      }
    }
    case "AllCrew": {
      const newState: GameState = focus(state,
        over(get, x => x.map(modify)),
      );
      return newState;
    }
    case "Positions": {
      return state;
    }
  }
}


*/