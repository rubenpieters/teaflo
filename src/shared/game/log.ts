import { Action } from "./action";
import { GameState } from "./state";
import { TriggerLog } from "./trigger";

export type LogEntry = {
  action: Action,
  state: GameState,
  transforms: TriggerLog[],
}

export type LogKeys = "st" | "fr" | "en";

export type Log = {
  [K in LogKeys]: LogEntry[]
};

export function emptyLog(): Log {
  return {
    st: [],
    fr: [],
    en: [],
  };
}