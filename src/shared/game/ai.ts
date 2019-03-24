import { focus, over, set } from "../iassign-util";
import { GameState } from "./state";
import { Intent } from "./intent";
import { GlobalId, UnitId } from "./entityId";

export type HasAI = {
  ai: AI,
  currentAI: number,
}

export type AI = { intent: Intent, spriteId: string, outs: Outs }[];

export type AIRoute = {
  aiOut: AIOut,
  condition: Condition,
};

export type Outs = AIRoute[];

export function routeText(
  route: AIRoute,
) {
  switch (route.aiOut.tag) {
    case "ToSelf": {
      return `self`;
    }
    case "ToX": {
      return `${route.aiOut.x}`;
    }
  }
}

export class ToX {
  constructor(
    public readonly x: number,
    public readonly tag: "ToX" = "ToX",
  ) {}
}

export class ToSelf {
  constructor(
    public readonly tag: "ToSelf" = "ToSelf",
  ) {}
}

type AIOut
  = ToX
  | ToSelf
  ;

type Condition = (state: GameState, self: UnitId) => boolean;

function nextOut(
  state: GameState,
  outs: Outs,
  self: UnitId,
): AIOut {
  const next: AIOut | undefined = outs.reduce((prev, { aiOut, condition }) => {
    if (prev === undefined) {
      if (condition(state, self)) {
        return aiOut;
      } else {
        return prev;
      }
    } else {
      return prev;
    }
  }, <AIOut | undefined>undefined);
  if (next === undefined) {
    console.log("nextOut: No condition applies");
    throw "nextOut: No condition applies";
  }
  return next;
}

export function nextAI<E extends HasAI>(
  state: GameState,
  e: E,
  self: UnitId,
): E {
  const newOut = nextOut(state, e.ai[e.currentAI].outs, self);
  switch (newOut.tag) {
    case "ToSelf": return e;
    case "ToX": return focus(e, set(x => x.currentAI, newOut.x));
  }
}

/**
 * positions:
 * 0 - 1 - 2
 * 3 - 4 - 5
 * 6 - 7 - 8
 */

export function aiPosition(
  index: number,
): { x: number, y: number } {
  const x = index % 3;
  const y =  Math.round((index / 3) - 0.5);
  return { x, y };
}

export type RouteDirection
  = "down"
  | "up"
  | "left"
  | "right"
  | "self"
  ;

export function routeDirection(
  from: number,
  to: number,
): RouteDirection {
  if (from === to) {
    return "self";
  } else if (from - 3 === to) {
    return "up";
  } else if (from + 3 === to) {
    return "down";
  } else if (from - 1 === to && from % 3 !== 0) {
    return "left";
  } else if (from + 1 === to && from % 3 !== 2) {
    return "right";
  } else {
    console.log("routeDirection: invalid direction");
    throw "routeDirection: invalid direction";
  }
}