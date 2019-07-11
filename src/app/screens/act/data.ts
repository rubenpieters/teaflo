import { GameRefs } from "../../states/game";
import { Solution, emptySolution } from "../../../shared/game/solution";
import { Location, emptyTree } from "../../../shared/tree";
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
  icon: string,
  iconLocation: { x: number, y: number },
  id: LevelDataKeys,
  cardIds: FrUnitId[][],
  enemyIds: EnUnitId[],
  slots: number,
  selectLocation: { x: number, y: number },
  boxSprite: string,
  supplyLocations: { x: number, y: number }[],
}

const a1l1: LevelData = {
  name: "A1 level1",
  id: "a1l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["a1l1_fr"]],
  enemyIds: ["a1l1_en"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }],
};

const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 250 },
  cardIds: [["a1l2_fr"]],
  enemyIds: ["a1l2_en"],
  slots: 1,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }],
};

const a1l3: LevelData = {
  name: "A1 level3",
  id: "a1l3",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["a1l3_fr1"], ["a1l3_fr2"]],
  enemyIds: ["a1l3_en1", "a1l3_en2"],
  slots: 2,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }],
};

const a1l4: LevelData = {
  name: "A1 level4",
  id: "a1l4",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 550, y: 250 },
  cardIds: [["a1l4_fr1"], ["a1l4_fr2"]],
  enemyIds: ["a1l4_en1", "a1l4_en2"],
  slots: 2,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }],
};

const a1l5: LevelData = {
  name: "A1 level5",
  id: "a1l5",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 400 },
  cardIds: [["a1l5_fr1"], ["a1l5_fr2"], ["a1l5_fr3"]],
  enemyIds: ["a1l5_en1", "a1l5_en2"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};

const a1l6: LevelData = {
  name: "A1 level6",
  id: "a1l6",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 400 },
  cardIds: [["a1l6_fr1"], ["a1l6_fr2"], ["a1l6_fr3"]],
  enemyIds: ["a1l6_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};

const a1l7: LevelData = {
  name: "A1 level7",
  id: "a1l7",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 400 },
  cardIds: [["a1l7_fr1"], ["a1l7_fr2"], ["a1l7_fr3"]],
  enemyIds: ["a1l7_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};


const a2l1: LevelData = {
  name: "A2 level1",
  id: "a2l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["trinity_dmg"], ["trinity_sup"], ["trinity_tnk"]],
  enemyIds: ["a2l1_en"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [{ x: 518, y: 27 }, { x: 70, y: 680 }, { x: 915, y: 715 }],
};

const a3l1: LevelData = {
  name: "A3 level1",
  id: "a3l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l1_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [
    { x: 100, y: 100 }, { x: 200, y: 100 }, { x: 300, y: 100 }, { x: 400, y: 100 },
    { x: 100, y: 200 }, { x: 200, y: 200 }, { x: 300, y: 200 },
    { x: 100, y: 300 }, { x: 200, y: 300 }, { x: 300, y: 300 }
  ],
};

const a3l2: LevelData = {
  name: "A3 level2",
  id: "a3l2",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l2_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [
    { x: 100, y: 100 }, { x: 200, y: 100 }, { x: 300, y: 100 }, { x: 400, y: 100 },
    { x: 100, y: 200 }, { x: 200, y: 200 }, { x: 300, y: 200 },
    { x: 100, y: 300 }, { x: 200, y: 300 }, { x: 300, y: 300 }
  ],
};

const a3l3: LevelData = {
  name: "A3 level3",
  id: "a3l3",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l3_en1", "a3l3_en2"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [
    { x: 100, y: 100 }, { x: 200, y: 100 }, { x: 300, y: 100 }, { x: 400, y: 100 },
    { x: 100, y: 200 }, { x: 200, y: 200 }, { x: 300, y: 200 },
    { x: 100, y: 300 }, { x: 200, y: 300 }, { x: 300, y: 300 }
  ],
};

const a3l4: LevelData = {
  name: "A3 level4",
  id: "a3l4",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 550, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l4_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [
    { x: 100, y: 100 }, { x: 200, y: 100 }, { x: 300, y: 100 }, { x: 400, y: 100 },
    { x: 100, y: 200 }, { x: 200, y: 200 }, { x: 300, y: 200 },
    { x: 100, y: 300 }, { x: 200, y: 300 }, { x: 300, y: 300 }
  ],
};

const a3l5: LevelData = {
  name: "A3 level5",
  id: "a3l5",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 700, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l5_en1"],
  slots: 3,
  selectLocation: { x: 200, y: 200 },
  boxSprite: "select3_f.png",
  supplyLocations: [
    { x: 100, y: 100 }, { x: 200, y: 100 }, { x: 300, y: 100 }, { x: 400, y: 100 },
    { x: 100, y: 200 }, { x: 200, y: 200 }, { x: 300, y: 200 },
    { x: 100, y: 300 }, { x: 200, y: 300 }, { x: 300, y: 300 }
  ],
};

export const actData: {
  [key: number]: ActData
} = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [a1l1, a1l2, a1l3, a1l4, a1l5, a1l6, a1l7],
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
    levels: [a3l1, a3l2, a3l3, a3l4, a3l5],
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
  "a1l5": a1l5,
  "a1l6": a1l6,
  "a1l7": a1l7,
  "a2l1": a2l1,
  "a3l1": a3l1,
  "a3l2": a3l2,
  "a3l3": a3l3,
  "a3l4": a3l4,
  "a3l5": a3l5,
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
  currentLevelId: LevelDataKeys | undefined,
  activeScreen: "menu" | "schem" | "codex" | "settings",
  levels: { [key in LevelDataKeys]?: Sol }
  animationSpeeds: {
    log: SpeedType,
  },
};

export type Sol = {
  solMap: SolutionDataMap,
  currentComposition: Composition,
}

export function mkActSaveData(): ActSaveData {
  return {
    currentMenu: undefined,
    currentLevelId: undefined,
    activeScreen: "menu",
    levels: {},
    animationSpeeds: {
      log: "play",
    }
  };
}

export type SolutionDataMap = {
  [key in string]?: SolutionData
};

export type Composition = (FrUnitId | undefined)[];

export function compositionToKey(
  composition: Composition
): string {
  return composition.map(x => {
    if (x !== undefined) {
      return x;
    } else {
      return "";
    }
  }).join(",");
}

export type SolutionData = {
  composition: Composition,
  solInfo: {
    solution: Solution,
    loc: Location
  },
};

export function mkSolutionData(
  levelId: LevelDataKeys,
): SolutionData {
  return {
    composition: emptyComposition(levelId),
    solInfo: {
      solution: emptySolution,
      loc: [],
    },
  };
}

export function emptyComposition(
  levelId: LevelDataKeys,
): Composition {
  const slots = levelData[levelId].slots;
  return repeat(slots, undefined);
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
      return new ScreenSchem(gameRefs.saveData.act.currentLevelId);
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
): LevelDataKeys | undefined {
  const menu = selectedMenu(gameRefs);
  if (menu === undefined) return undefined;
  switch (menu.tag) {
    case "SelectedActMenu": return undefined;
    case "SelectedLevelMenu": return menu.levelId;
  } 
}

export function selectedSchemLevelId(
  gameRefs: GameRefs,
): LevelDataKeys | undefined {
  return gameRefs.saveData.act.currentLevelId;
}

export function selectedSchemComposition(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
): Composition | undefined {
  const sol = gameRefs.saveData.act.levels[levelId];
  return sol === undefined ? undefined : sol.currentComposition;
}

export function currentSol(
  gameRefs: GameRefs,
): Sol | undefined {
  const levelId = selectedSchemLevelId(gameRefs);
  return levelId === undefined ? undefined : gameRefs.saveData.act.levels[levelId];
}

export function currentSolMap(
  gameRefs: GameRefs,
): SolutionDataMap | undefined {
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId !== undefined) {
    const sol = gameRefs.saveData.act.levels[levelId];
    return sol === undefined ? undefined : sol.solMap;
  } else {
    return undefined;
  }
}

export function initSol(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
): Sol {
  const newSol = { currentComposition: emptyComposition(levelId), solMap: {} };
  gameRefs.saveData.act.levels[levelId] = newSol;
  return newSol;
}

export function schemScholAt(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
  composition: Composition,
): SolutionData | undefined {
  return gameRefs.saveData.act.levels[levelId]!.solMap[compositionToKey(composition)];
}

export function currentSolution(
  gameRefs: GameRefs,
) {
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId !== undefined) {
    const composition = selectedSchemComposition(gameRefs, levelId);
    if (composition !== undefined) {
      const solution = gameRefs.saveData.act.levels[levelId]!.solMap[compositionToKey(composition)];
      return solution === undefined ? undefined : solution.solInfo;
    } else {
      return undefined;
    }
  } else {
    return undefined;
  }
}

export function setSolution(
  gameRefs: GameRefs,
  solInfo: { solution: Solution, loc: Location },
) {
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId !== undefined) {
    const composition = selectedSchemComposition(gameRefs, levelId);
    if (composition !== undefined) {
      const solution = gameRefs.saveData.act.levels[levelId]!.solMap[compositionToKey(composition)];
      if (solution !== undefined) {
        solution.solInfo = solInfo;
      }
    }
  }
}

export function initSolution(
  gameRefs: GameRefs,
): SolutionData | undefined {
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId !== undefined) {
    const composition = selectedSchemComposition(gameRefs, levelId);
    if (composition !== undefined) {
      const newSolution = {
        composition,
        solInfo: {
          solution: emptySolution,
          loc: [],
        }
      };
      gameRefs.saveData.act.levels[levelId]!.solMap[compositionToKey(composition)] = newSolution;
      return newSolution;
    } else {
      return undefined;
    }
  } else {
    return undefined;
  }
}

export function setLocation(
  gameRefs: GameRefs,
  loc: Location,
) {
  const levelId = selectedSchemLevelId(gameRefs);
  if (levelId !== undefined) {
    const composition = selectedSchemComposition(gameRefs, levelId);
    if (composition !== undefined) {
      const solInfo = gameRefs.saveData.act.levels[levelId]!.solMap[compositionToKey(composition)]!.solInfo;
      if (solInfo !== undefined) {
        solInfo.loc = loc;
      } 
    }
  }
}

export function validComposition(
  composition: Composition,
  levelId: LevelDataKeys,
) {
  const slots = levelData[levelId].slots;
  return composition.length === slots && composition.findIndex(x => x === undefined) === -1;
}