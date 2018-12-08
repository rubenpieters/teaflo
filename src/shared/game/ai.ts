import { focus, over, set } from "src/shared/iassign-util";
import { Action } from "./action";
import { GameState } from "./state";

export type HasAI = {
  ai: AI,
  currentAI: number,
}

export type AI = { action: Action, outs: Outs }[];

type Outs = { aiOut: AIOut, condition: Condition }[];

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

type Condition = (state: GameState) => boolean;

function nextOut(
  state: GameState,
  outs: Outs,
): AIOut {
  const next: AIOut | undefined = outs.reduce((prev, { aiOut, condition }) => {
    if (prev === undefined) {
      if (condition(state)) {
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
  e: E
): E {
  const newOut = nextOut(state, e.ai[e.currentAI].outs);
  switch (newOut.tag) {
    case "ToSelf": return e;
    case "ToX": return focus(e, set(x => x.currentAI, newOut.x));
  }
}