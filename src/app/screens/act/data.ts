import { GameRefs } from "src/app/states/game";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
}

export type LevelData = {
  name: string,
  id: string,
}

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [
      {
        name: "A1 level1",
        id: "a1l1",
      },
      {
        name: "A1 level2",
        id: "a1l2",
      },
    ],
  },
  1: {
    shortName: "2",
    longName: "Act 2",
    levels: [
      {
        name: "A2 level1",
        id: "a2l1",
      },
    ],
  },
  2: {
    shortName: "3",
    longName: "Act 3",
    levels: [
      {
        name: "A3 level1",
        id: "a3l1",
      },
    ],
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

export type LevelSaveData = {
  selectedLevelId: string | undefined,
  levels: { [key in string]: SolutionData[]
  },
}

export function mkLevelSaveData(): LevelSaveData {
  return {
    selectedLevelId: undefined,
    levels: {},
  };
}

export function selectedLevelId(
  gameRefs: GameRefs
): string | undefined {
  return gameRefs.saveData.level.selectedLevelId;
}

export type SolutionData = {
  name: string,
}

export function mkSolutionData(
): SolutionData {
  return {
    name: "New_Sol",
  };
}