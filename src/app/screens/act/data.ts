import { GameRefs } from "../../states/game";
import { Solution } from "../../../shared/game/solution";
import { Location } from "../../../shared/tree";
import { SpeedType } from "../../../app/phaser/animation";
import { ScreenActive, ScreenAct, ScreenSchem, ScreenCodex, ScreenSettings } from "../transition";
import { FrUnitId } from "../../../shared/data/frUnitMap";
import { EnUnitId } from "../../../shared/data/enUnitMap";
import { isTrue, Equal } from "../../../shared/type-util";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
  bgSprite: string,
}

export type LevelData = {
  name: string,
  id: LevelDataKeys,
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
  cardIds: ["a1l1_fr"],
  enemyIds: ["a1l1_en"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }],
};

const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  cardIds: ["a1l2_fr"],
  enemyIds: ["a1l2_en"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }],
};

const a1l3: LevelData = {
  name: "A1 level3",
  id: "a1l3",
  cardIds: ["a1l3_fr1", "a1l3_fr2"],
  enemyIds: ["a1l3_en1", "a1l3_en2"],
  slots: 2,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }],
};

const a1l4: LevelData = {
  name: "A1 level4",
  id: "a1l4",
  cardIds: ["a1l4_fr1", "a1l4_fr2"],
  enemyIds: ["a1l4_en1", "a1l4_en2"],
  slots: 2,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }],
};

const a2l1: LevelData = {
  name: "A2 level1",
  id: "a2l1",
  cardIds: ["trinity_dmg", "trinity_sup", "trinity_tnk"],
  enemyIds: ["a2l1_en"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};

const a3l1: LevelData = {
  name: "A3 level1",
  id: "a3l1",
  cardIds: [],
  enemyIds: [],
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
    levels: [a1l1, a1l2, a1l3, a1l4],
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



// check that values of levelData are all `LevelData`
type LevelDataValues = (typeof levelData)[keyof (typeof levelData)];
isTrue<Equal<LevelDataValues, LevelData>>(true);
export type LevelDataKeys = keyof (typeof levelData);

export const levelData = {
  "a1l1": a1l1,
  "a1l2": a1l2,
  "a1l3": a1l3,
  "a1l4": a1l4,
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
    public readonly levelId: LevelDataKeys,
    public readonly tag: "SelectedLevelMenu" = "SelectedLevelMenu",
  ) {}
}

export type ActSaveData = {
  currentMenu: SelectedActMenu | SelectedLevelMenu | undefined,
  currentSchem: SelectedBuildSchem | SelectedExecSchem | undefined,
  activeScreen: "menu" | "schem" | "codex" | "settings",
  levels: { [key in LevelDataKeys]?: SolutionData[] },
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
  levelId: LevelDataKeys,
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
    public readonly levelId: LevelDataKeys,
    public readonly solId: number,
    public readonly tag: "SelectedBuildSchem" = "SelectedBuildSchem",
  ) {}
}

export class SelectedExecSchem {
  constructor (
    public readonly levelId: LevelDataKeys,
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
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[schem.levelId]![schem.solId];
}

export function schemScholAt(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
  solId: number,
): SolutionData | undefined {
  const schem = selectedSchem(gameRefs);
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[levelId]![solId];
}

export function currentSolution(
  gameRefs: GameRefs,
) {
  const schem = selectedSchem(gameRefs);
  return schem === undefined ? undefined : gameRefs.saveData.act.levels[schem.levelId]![schem.solId].solInfo;
}

export function setSolution(
  gameRefs: GameRefs,
  solInfo: { solution: Solution, loc: Location },
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    gameRefs.saveData.act.levels[schem.levelId]![schem.solId].solInfo = solInfo;
  }
}

export function setLocation(
  gameRefs: GameRefs,
  loc: Location,
) {
  const schem = selectedSchem(gameRefs);
  if (schem !== undefined) {
    const solInfo = gameRefs.saveData.act.levels[schem.levelId]![schem.solId].solInfo;
    if (solInfo !== undefined) {
      solInfo.loc = loc;
    }
  }
}