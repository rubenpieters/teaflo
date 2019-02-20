import { Action } from "./action";
import { GameState } from "./state";
import { TriggerLog } from "./trigger";
import { Omit } from "../type-util";

export type LogEntry = {
  action: Action,
  state: GameState,
  transforms: TriggerLog[],
}

export type LogKeys = "st" | "fr" | "en";

export type LogKeySt = {
  index: number,
  type: "st",
}

export type LogKeyFr = {
  index: number,
  type: "fr",
}

export type LogKeyEn = {
  index: number,
  enIndex: number,
  type: "en",
}

export type LogIndex
  = LogKeySt
  | LogKeyFr
  | LogKeyEn
  ;

// TODO: this assumes that logs are non-empty
export function getPrevLogEntry(
  log: Log,
  logIndex: LogIndex,
): LogEntry | undefined {
  if (logIndex.index === 0) {
    switch (logIndex.type) {
      case "st": return undefined;
      case "fr": return log.st[log.st.length - 1];
      case "en": {
        if (logIndex.enIndex === 0) {
          return log.fr[log.fr.length - 1];
        } else {
          const prevEnLog = log.en[logIndex.enIndex - 1];
          return prevEnLog[prevEnLog.length - 1];
        }
      }
    }
  } else {
    if (logIndex.type === "en") {
      return log[logIndex.type][logIndex.enIndex][logIndex.index - 1];
    } else {
      return log[logIndex.type][logIndex.index - 1];
    }
  }
  throw `getPrevLogEntry -- impossible ${JSON.stringify(logIndex)}`;
}

export function getLogEntry(
  log: Log,
  logIndex: LogIndex,
): LogEntry {
  if (logIndex.type === "en") {
    return log[logIndex.type][logIndex.enIndex][logIndex.index];
  }
  return log[logIndex.type][logIndex.index];
}

export function logIndexLt(
  logIndex1: LogIndex,
  logIndex2: LogIndex,
): boolean {
  const logTypes: ["st", "fr", "en"] = ["st", "fr", "en"];
  if (logTypes.indexOf(logIndex1.type) < logTypes.indexOf(logIndex2.type)) {
    return true;
  }
  if (logTypes.indexOf(logIndex1.type) > logTypes.indexOf(logIndex2.type)) {
    return false;
  }
  if (
    logIndex1.type === "en" && logIndex2.type === "en" &&
    logIndex1.enIndex < logIndex2.enIndex
  ) {
    return true;
  }
  if (
    logIndex1.type === "en" && logIndex2.type === "en" &&
    logIndex1.enIndex > logIndex2.enIndex
  ) {
    return false;
  }
  if (logIndex1.index < logIndex2.index) {
    return true;
  }
  return false;
}

export function logIndexEq(
  logIndex1: LogIndex,
  logIndex2: LogIndex,
): boolean {
  const logTypes: ["st", "fr", "en"] = ["st", "fr", "en"];
  if (logTypes.indexOf(logIndex1.type) < logTypes.indexOf(logIndex2.type)) {
    return false;
  }
  if (logTypes.indexOf(logIndex1.type) > logTypes.indexOf(logIndex2.type)) {
    return false;
  }
  if (
    logIndex1.type === "en" && logIndex2.type === "en" &&
    logIndex1.enIndex < logIndex2.enIndex
  ) {
    return false;
  }
  if (
    logIndex1.type === "en" && logIndex2.type === "en" &&
    logIndex1.enIndex > logIndex2.enIndex
  ) {
    return false;
  }
  if (logIndex1.index === logIndex2.index) {
    return true;
  }
  return false;
}

export function allLogIndices(
  state: GameState,
  log: Log,
): { logIndex: LogIndex, typeIndex: number, entryIndex: number }[] {
  let x: LogIndex | undefined = { type: "st", index: -1 };
  let l: { logIndex: LogIndex, typeIndex: number, entryIndex: number }[] = [];
  let prevTypeIndex = 0;
  let entryIndex = 0;
  while (x !== undefined) {
    x = nextLogKey(state, log, x);
    if (x !== undefined) {
      if (prevTypeIndex !== logTypeIndex(x)) {
        prevTypeIndex = logTypeIndex(x);
        entryIndex = 0;
      } else {
        entryIndex += 1;
      }
      l.push({
        logIndex: x,
        typeIndex: logTypeIndex(x),
        entryIndex,
      });
    }
  }
  return l;
}

export function logTypeIndex(
  logIndex: LogIndex
): number {
  switch (logIndex.type) {
    case "st": return 0;
    case "fr": return 1;
    case "en": return logIndex.enIndex + 2;
  }
}

export function firstLogKey(
  state: GameState,
  log: Log,
): LogIndex | undefined {
  return nextLogKey(state, log, { type: "st", index: -1 });
}

export function nextLogKey(
  state: GameState,
  log: Log,
  index: LogIndex,
): LogIndex | undefined {
  const logTypes: ["st", "fr", "en"] = ["st", "fr", "en"];
  let len: number;
  if (index.type === "en") {
    if (log[index.type][index.enIndex] === undefined) {
      len = 0;
    } else {
      len = log[index.type][index.enIndex].length;
    }
  } else {
    len = log[index.type].length;
  }
  const newIndex = index.index + 1;
  if (newIndex >= len) {
    // the newIndex exceeds the log length
    if (index.type === "en") {
      const enemyCount = state.enUnits.length;
      const newEnIndex = index.enIndex + 1;
      if (newEnIndex < enemyCount) {
        // the newEnIndex does not exceed the enemy count
        return nextLogKey(state, log, <LogKeyEn>{
          type: "en",
          index: -1,
          enIndex: newEnIndex,
        });
      }
      // else: fallthrough to the next part, which increases the type of the index
    }
    // we go to the next log type
    const newLogTypeIndex = logTypes.indexOf(index.type) + 1;
    if (newLogTypeIndex >= logTypes.length) {
      return undefined;
    } else {
      if (logTypes[newLogTypeIndex] === "en") {
        return nextLogKey(state, log, <LogKeyEn>{
          type: logTypes[newLogTypeIndex],
          index: -1,
          enIndex: 0,
        });
      } else {
        return nextLogKey(state, log, <LogKeySt | LogKeyFr>{
          type: logTypes[newLogTypeIndex],
          index: -1,
        });
      }
    }
  } else {
    // the newIndex does not exceed the index log
    // we keep all other parameters
    if (index.type === "en") {
      return <LogKeyEn>{
        type: index.type,
        index: newIndex,
        enIndex: index.enIndex,
      };
    } else {
      return <LogKeySt | LogKeyFr>{
        type: index.type,
        index: newIndex,
      };
    }
  }
}

export type Log = {
  st: LogEntry[]
  fr: LogEntry[]
  en: {
    // a list of log entries for each enemy
    [key: number]: LogEntry[]
  }
};

export function emptyLog(): Log {
  return {
    st: [],
    fr: [],
    en: [],
  };
}