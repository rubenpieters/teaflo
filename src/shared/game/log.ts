import { Action } from "./action";
import { GameState } from "./state";
import { TriggerLog } from "./trigger";

export type LogEntry = {
  action: Action,
  state: GameState,
  transforms: TriggerLog[],
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