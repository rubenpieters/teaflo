import { Action } from "./action";
import { GameState } from "./state";

export type LogEntry = {
  action: Action,
  state: GameState,
}

export type Log = LogEntry[];

export function emptyLog() {
  return [];
}