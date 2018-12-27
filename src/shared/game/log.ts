import { Action } from "./action";
import { GameState } from "./state";

export type LogEntry = {
  action: Action,
  state: GameState,
}

export type Log = {
  frLog: LogEntry[],
  enLog: LogEntry[],
};

export function emptyLog(): Log {
  return {
    frLog: [],
    enLog: [],
  };
}