import { Action } from "./action";
import { GameState } from "./state";
import { TriggerLog } from "./trigger";
import { Omit } from "../type-util";
import { GlobalId } from "./entityId";
import { Context } from "./intent";

export type Log = LogEntry[];

export type LogEntry = {
  action: Action,
  state: GameState,
  transforms: TriggerLog[],
  originalContext: Context,
  context: Context,
  entryIndex: number, // start turn: 0, friendly action: 1, then +1 for each enemy action
  typeIndex: number,
  actionIndex: number, // on original action: 0, then +1 for each trigger
};

export type LogEntryI = LogEntry & { logIndex: number };

export type LogKeySt = {
  type: "st",
};

export type LogKeyFr = {
  type: "st",
  frIndex: number,
};

export type LogKeyEn = {
  type: "en",
  enIndex: number,
};

export type LogKey
  = LogKeySt
  | LogKeyFr
  | LogKeyEn
  ;

export type LogIndex = number;

export function emptyLog(): Log {
  return [];
}

export function getLogEntry(
  log: Log,
  logIndex: LogIndex,
): LogEntry {
  return log[logIndex];
}

export function allLogIndices(
  log: Log,
): LogEntryI[] {
  return log.map((logEntry, i) => {
    return { ...logEntry, logIndex: i }
  });
}

export function logIndexLt(
  logIndex1: LogIndex,
  logIndex2: LogIndex,
): boolean {
  return logIndex1 < logIndex2;
}

export function logIndexEq(
  logIndex1: LogIndex,
  logIndex2: LogIndex,
): boolean {
  return logIndex1 === logIndex2;
}

export function getPrevLogEntry(
  log: Log,
  logIndex: LogIndex,
): LogEntry | undefined {
  if (logIndex === 0) {
    return undefined;
  }
  return log[logIndex - 1];
}

export function firstLogIndex(
): LogIndex {
  return 0;
}

export function nextLogIndex(
  log: Log,
  logIndex: LogIndex,
): LogIndex | undefined {
  const newIndex = logIndex + 1;
  if (newIndex >= log.length) {
    return undefined;
  }
  return newIndex;
}