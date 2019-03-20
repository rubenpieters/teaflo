import { GameRefs } from "../../states/game";
import { Solution } from "../../../shared/game/solution";
import { Location } from "../../../shared/tree";
import { SpeedType } from "../../../app/phaser/animation";
import { ScreenActive, ScreenAct, ScreenSchem, ScreenCodex, ScreenSettings } from "../transition";
import { FrUnitId } from "../../../shared/data/units/friendly";
import { EnUnitId } from "../../../shared/data/units/enemy";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
  bgSprite: string,
}

export type LevelData = {
  name: string,
  id: string,
  cardIds: FrUnitId[],
  enemyIds: EnUnitId[],
  slots: number,
  selectLocation: { x: number, y: number },
  boxSprite: string,
  supplyLocations: { x: number, y: number }[],
}

const a1l1: LevelData = {
  name: "A1 level1",
  id: "a1l1",
  cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"],
  enemyIds: ["en_unit_a1_l2_01"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};

const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"],
  enemyIds: ["en_unit_a1_l3_01", "en_unit_a1_l3_02"],
  slots: 3,
  selectLocation: { x: 500, y: 500 },
  boxSprite: "select3.png",
  supplyLocations: [{ x: 300, y: 100 }, { x: 150, y: 500 }, { x: 450, y: 500 }],
};

const a1l3: LevelData = {
  name: "A1 level3",
  id: "a1l3",
  cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"],
  enemyIds: ["en_unit_a1_l4_01"],
  slots: 2,
  selectLocation: { x: 500, y: 500 },
  boxSprite: "select3.png",
  supplyLocations: [{ x: 300, y: 100 }, { x: 150, y: 500 }, { x: 450, y: 500 }],
};

const a2l1: LevelData = {
  name: "A2 level1",
  id: "a2l1",
  cardIds: [],
  enemyIds: ["en_unit_a1_l2_01"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3.png",
  supplyLocations: [{ x: 300, y: 100 }, { x: 150, y: 500 }, { x: 450, y: 500 }],
};

const a3l1: LevelData = {
  name: "A3 level1",
  id: "a3l1",
  cardIds: ["test4ab"],
  enemyIds: ["en_unit_a1_l2_01"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3.png",
  supplyLocations: [{ x: 300, y: 100 }, { x: 150, y: 500 }, { x: 450, y: 500 }],
};

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [a1l1, a1l2, a1l3],
    bgSprite: "sel_act1.png",
  },
  1: {
    shortName: "2",
    longName: "Act 2",
    levels: [a2l1],
    bgSprite: "sel_act1.png",
  },
  2: {
    shortName: "3",
    longName: "Act 3",
    levels: [a3l1],
    bgSprite: "sel_act1.png",
  },
}

export const levelData: {
  [key: string]: LevelData
} = {
  "a1l1": a1l1,
  "a1l2": a1l2,
  "a1l3": a1l3,
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
  activeScreen: "menu" | "schem" | "codex" | "settings",
  levels: { [key in string]: SolutionData[] },
  animationSpeeds: {
    log: SpeedType,
  },
}
export function mkActSaveData(): ActSaveData {
  return {
    currentMenu: undefined,
    currentSchem: undefined,
    activeScreen: "menu",
    levels: {},
    animationSpeeds: {
      log: "play",
    }
  };
}

export type SolutionData = {
  name: string,
  supply: { cardId: FrUnitId, deployPos: number | undefined }[],
  solInfo: {
    solution: Solution,
    loc: Location
  } | undefined,
}

export function mkSolutionData(
  levelId: string,
): SolutionData {
  const supply = levelData[levelId].cardIds.map(cardId => { return { cardId, deployPos: undefined }});
  return {
    name: "New_Sol",
    supply,
    solInfo: undefined,
  };
}

function repeat<A>(
  x: number,
  a: A,
): A[] {
  const l: A[] = [];
  for (let i = 0; i < x; i++) {
    l.push(a);
  }
  return l;
}

export function currentScreen(
  gameRefs: GameRefs
): ScreenActive {
  switch (gameRefs.saveData.act.activeScreen) {
    case "menu": {
      return new ScreenAct(gameRefs.saveData.act.currentMenu);
    }
    case "schem": {
      return new ScreenSchem(gameRefs.saveData.act.currentSchem);
    }
    case "codex": {
      return new ScreenCodex();
    }
    case "settings": {
      return new ScreenSettings();
    }
  }
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
  gameRefs: GameRefs,
): SelectedBuildSchem | SelectedExecSchem | undefined {
  return gameRefs.saveData.act.currentSchem;
}

export function currentSchemSol(
  gameRefs: GameRefs,
): SolutionData | undefined {
  const schem = selectedSchem(gameRefs);
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[schem.levelId][schem.solId];
}

export function schemScholAt(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
): SolutionData | undefined {
  const schem = selectedSchem(gameRefs);
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[levelId][solId];
}

export function currentSolution(
  gameRefs: GameRefs,
) {
  const schem = selectedSchem(gameRefs);
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[schem.levelId][schem.solId].solInfo;
}

export function setSolution(
  gameRefs: GameRefs,
  solInfo: { solution: Solution, loc: Location },
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    gameRefs.saveData.act.levels[schem.levelId][schem.solId].solInfo = solInfo;
  }
}

export function setLocation(
  gameRefs: GameRefs,
  loc: Location,
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    const solInfo = gameRefs.saveData.act.levels[schem.levelId][schem.solId].solInfo;
    if (solInfo !== undefined) {
      solInfo.loc = loc;
    }
  }
}