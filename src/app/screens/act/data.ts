import { GameRefs } from "src/app/states/game";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
}

export type LevelData = {
  name: string,
  id: string,
  cardIds: string[],
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
        cardIds: [],
      },
      {
        name: "A1 level2",
        id: "a1l2",
        cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"],
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
        cardIds: [],
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
        cardIds: [],
      },
    ],
  },
}

export class SelectedAct {
  constructor (
    public readonly actId: number,
    public readonly tag: "SelectedAct" = "SelectedAct",
  ) {}
}

export class SelectedLevel {
  constructor (
    public readonly levelId: string,
    public readonly tag: "SelectedLevel" = "SelectedLevel",
  ) {}
}

export type ActSaveData = {
  currentMenu: SelectedAct | SelectedLevel | undefined,
  levels: { [key in string]: SolutionData[] }
}

export function mkActSaveData(): ActSaveData {
  return {
    currentMenu: undefined,
    levels: {},
  };
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

export function selectedMenu(
  gameRefs: GameRefs
): SelectedAct | SelectedLevel | undefined {
  return gameRefs.saveData.act.currentMenu;
}

export function selectedActId(
  gameRefs: GameRefs
): number | undefined {
  const menu = selectedMenu(gameRefs);
  if (menu === undefined) return undefined;
  switch (menu.tag) {
    case "SelectedAct": return menu.actId;
    case "SelectedLevel": return undefined;
  } 
}

export function selectedLevelId(
  gameRefs: GameRefs
): string | undefined {
  const menu = selectedMenu(gameRefs);
  if (menu === undefined) return undefined;
  switch (menu.tag) {
    case "SelectedAct": return undefined;
    case "SelectedLevel": return menu.levelId;
  } 
}