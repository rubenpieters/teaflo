import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";

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

export type AllCrew = {
  tag: "AllCrew",
};

export type TargetSpec
  = Self
  | AllCrew
  ;

export type Positions = {
  tag: "Positions",
  positions: number[],
};

export type TargetId = {
  tag: "TargetId",
  id: number,
};

export type Target
  = Positions
  | TargetId
  | AllCrew
  ;

export function findTarget(
  targetSpec: TargetSpec,
  selfId: number,
): Target {
  switch (targetSpec.tag) {
    case "Self": {
      return {
        tag: "TargetId",
        id: selfId,
      };
    }
    case "AllCrew": {
      return targetSpec;
    }
  }
}

export function onTargets(
  target: Target,
  f: (crew: IdCrew) => IdCrew,
  state: GameState,
): GameState {
  switch (target.tag) {
    case "TargetId": {
      const targetIndex = indexOfId(target.id, state.crew);
      if (targetIndex === "notFound") {
        return state;
      } else {
        return focus(state,
          over(x => x.crew[targetIndex], f)
        );
      }
    }
    case "AllCrew": {
      const newState: GameState = focus(state,
        over(x => x.crew, x => x.map(f))
      );
      return newState;
    }
    case "Positions": {
      return state;
    }
  }
}

export function indexOfId(
  id: number,
  crew: IdCrew[],
) {
  let index: number = 0;
  for (const ally of crew) {
    if (ally.id === id) {
      return index;
    }
    index += 1;
  }
  return "notFound";
}