import { focus, over, set } from "src/shared/iassign-util";
import { GameState, Id, IdCrew, IdItem } from "src/shared/game/state";

// Action<TargetInfo>
// Target<TargetInfo> (State => TargetInfo)

/*
type TargetFunction<S, T> = (s: S) => T;
type TF = <R>(e: <S>(s: S) => R) => R;

const tf: TF = e => {
  return e((s: { gs: GameState, id: number }) => {
    const id = indexOfId(s.id, s.gs.crew);
    if (id === "notFound") {
      throw "";
    } else {
      return s.gs.crew[id];
    }
  });
}
*/

export type Self = {
  tag: "Self",
};

export type AllCrewSpec = {
  tag: "AllCrew",
};

export type LastCrew = {
  tag: "LastCrew",
};

export type TargetSpec
  = Self
  | AllCrewSpec
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

export type Target
  = Positions
  | TargetId
  | AllCrew
  ;

export type TargetType = "ally" | "enemy" | "item";

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