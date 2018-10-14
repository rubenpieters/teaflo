import { GameState } from "src/shared/game/state";

export type HasNext = {
  next: Next,
}

type NextId = {
  tag: "NextId",
}

type Repeat = {
  tag: "Repeat",
}

type Goto = {
  tag: "Goto",
  action: number,
}

type NextCondition = {
  tag: "NextCondition",
  condition: (state: GameState) => boolean,
  ifT: Next,
  ifF: Next,
}

export type Next
  = NextId
  | Repeat
  | Goto
  | NextCondition
