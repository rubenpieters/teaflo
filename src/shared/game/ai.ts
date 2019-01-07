import { focus, over, set } from "src/shared/iassign-util";
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

type Outs = AIRoute[];

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