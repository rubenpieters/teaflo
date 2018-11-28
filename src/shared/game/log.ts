import { Action } from "./action";

export type Log = {
  frAction: Action[],
  enAction: Action[],
}

export function emptyLog() {
  return {
    frAction: [],
    enAction: [],
  }
}