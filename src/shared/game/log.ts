import { ActionWithOrigin, Action } from "../definitions/action";
import { GameState } from "../definitions/state";
import { StatusTag } from "../definitions/status";
import { StatusId } from "../definitions/entityId";

export class AbilitySource {
  public readonly tag: "AbilitySource" = "AbilitySource";

  constructor(
    public readonly id: number,
  ) {}
}

export class StatusSource {
  public readonly tag: "StatusSource" = "StatusSource";

  constructor(
    public readonly id: number,
    public readonly statusId: StatusId,
  ) {}
}

export class RuleSource {
  public readonly tag: "RuleSource" = "RuleSource";

  constructor(
    public readonly id: number,
  ) {}
}

export type ActionSource
  = AbilitySource
  | StatusSource
  | RuleSource
  ;

export type Log = LogEntry[];

export type LogEntry = {
  action: ActionWithOrigin,
  state: GameState,
  transforms: StatusLog[],
  entryIndex: number, // entry within ability
  typeIndex: number, // start turn: 0, friendly action: 1, then +1 for each enemy action
  actionIndex: number, // on original action: 0, then +1 for each trigger
  actionSource: ActionSource, // index within ability, plus tag indicating source of action
  intermediateIndex: number, // overall index
};

export type StatusLog = {
  tag: StatusTag,
  before: Action,
  after: Action,
};

export type LogEntryI = LogEntry & { logIndex: number };

export type LogKeySt = {
  type: "st",
};

export type LogKeyFr = {
  type: "fr",
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

export function splitLog(
  log: Log,
): { [K in number]: LogEntryI[] } {
  let obj: { [K in number]: LogEntryI[] } = {};
  log.forEach((entry, i) => {
    if (obj[entry.typeIndex] === undefined) {
      obj[entry.typeIndex] = [{ ...entry, logIndex: i }];
    } else {
      obj[entry.typeIndex].push({ ...entry, logIndex: i });
    }
  });
  return obj;
}