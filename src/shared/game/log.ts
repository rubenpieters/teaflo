import { Action } from "./action";
import { GameState } from "./state";

export type LogEntry = {
  action: Action,
  state: GameState,
}

export type Log = {
  frAction: LogEntry[],
  enAction: LogEntry[],
}

export function emptyLog() {
  return {
    frAction: [],
    enAction: [],
  }
}