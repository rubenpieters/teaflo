import { GameRefs } from "../../states/game";

export type LevelData = {
  name: string,
  id: string,
}

export type LevelMap = {
  [key: number]: LevelData[]
}

export const levelMap: LevelMap = {
  0: [
    {
      name: "A1 level1",
      id: "a1l1",
    },
    {
      name: "A1 level2",
      id: "a1l2",
    },
  ],
  1: [
    {
      name: "A2 level1",
      id: "a2l1",
    },
  ],
  2: [
    {
      name: "A3 level1",
      id: "a3l1",
    },
  ],
}

export type LevelSelectSaveData = {
  selectedLevelId: {[key: number]: string},
}

export function mkLevelSaveData(): LevelSelectSaveData {
  return {
    selectedLevelId: {},
  };
}

export function selectedLevelId(
  gameRefs: GameRefs,
  actId: number,
): string | undefined {
  return gameRefs.saveData.levelSelect.selectedLevelId[actId];
}