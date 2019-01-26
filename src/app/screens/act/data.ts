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

const a1l1: LevelData = {
  name: "A1 level1",
  id: "a1l1",
  cardIds: [],
};

const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"],
};

const a2l1: LevelData = {
  name: "A2 level1",
  id: "a2l1",
  cardIds: [],
};

const a3l1: LevelData = {
  name: "A3 level1",
  id: "a3l1",
  cardIds: [],
};

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [a1l1, a1l2],
  },
  1: {
    shortName: "2",
    longName: "Act 2",
    levels: [a2l1],
  },
  2: {
    shortName: "3",
    longName: "Act 3",
    levels: [a3l1],
  },
}

export const levelData: {
  [key: string]: LevelData
} = {
  "a1l1": a1l1,
  "a1l2": a1l2,
  "a2l1": a2l1,
  "a3l1": a3l1,
}

export class SelectedActMenu {
  constructor (
    public readonly actId: number,
    public readonly tag: "SelectedActMenu" = "SelectedActMenu",
  ) {}
}

export class SelectedLevelMenu {
  constructor (
    public readonly levelId: string,
    public readonly tag: "SelectedLevelMenu" = "SelectedLevelMenu",
  ) {}
}

export type ActSaveData = {
  currentMenu: SelectedActMenu | SelectedLevelMenu | undefined,
  currentSchem: SelectedBuildSchem | SelectedExecSchem | undefined,
  levels: { [key in string]: SolutionData[] }
}

export function mkActSaveData(): ActSaveData {
  return {
    currentMenu: undefined,
    currentSchem: undefined,
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
): SelectedActMenu | SelectedLevelMenu | undefined {
  return gameRefs.saveData.act.currentMenu;
}

export function selectedActId(
  gameRefs: GameRefs
): number | undefined {
  const menu = selectedMenu(gameRefs);
  if (menu === undefined) return undefined;
  switch (menu.tag) {
    case "SelectedActMenu": return menu.actId;
    case "SelectedLevelMenu": return undefined;
  } 
}

export function selectedLevelId(
  gameRefs: GameRefs
): string | undefined {
  const menu = selectedMenu(gameRefs);
  if (menu === undefined) return undefined;
  switch (menu.tag) {
    case "SelectedActMenu": return undefined;
    case "SelectedLevelMenu": return menu.levelId;
  } 
}

export class SelectedBuildSchem {
  constructor (
    public readonly levelId: string,
    public readonly solId: number,
    public readonly tag: "SelectedBuildSchem" = "SelectedBuildSchem",
  ) {}
}

export class SelectedExecSchem {
  constructor (
    public readonly levelId: string,
    public readonly solId: number,
    public readonly tag: "SelectedExecSchem" = "SelectedExecSchem",
  ) {}
}

export function selectedSchem(
  gameRefs: GameRefs
): SelectedBuildSchem | SelectedExecSchem | undefined {
  return gameRefs.saveData.act.currentSchem;
}

export function levelAvailableCards(
  gameRefs: GameRefs
): string[] | undefined {
  const schem = selectedSchem(gameRefs);
  if (schem === undefined) return undefined;
  console.log(`LEVELID: ${schem.levelId}`);
  switch (schem.tag) {
    case "SelectedBuildSchem": return levelData[schem.levelId].cardIds;
    case "SelectedExecSchem": return undefined;
  } 
}