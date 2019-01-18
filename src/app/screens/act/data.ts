import { GameRefs } from "src/app/states/game";

export type ActData = {
  shortName: string,
  longName: string,
}

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
  },
  1: {
    shortName: "2",
    longName: "Act 2",
  },
  2: {
    shortName: "3",
    longName: "Act 3",
  },
}

export type ActSaveData = {
  selectedActId: number | undefined,
}

export function mkActSaveData(): ActSaveData {
  return {
    selectedActId: undefined,
  };
}

export function selectedActId(
  gameRefs: GameRefs
): number | undefined {
  return gameRefs.saveData.act.selectedActId;
}