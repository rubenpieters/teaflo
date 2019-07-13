import { LevelDataKeys, levelData, LevelData } from "./levelData";
import { SpeedType } from "../phaser/animation";
import { FrUnitId } from "../../shared/data/frUnitMap";
import { Solution, emptySolution } from "../../shared/game/solution";
import { repeat } from "../util/util";
import { GameRefs } from "../states/game";
import { ScreenActive, ScreenAct, ScreenExec, ScreenCodex, ScreenSettings } from "../screens/transition";
import { ActDataKeys } from "./actData";
import { Location } from "../../shared/tree";

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

export type LevelSaveData = {
  solMap: SolutionDataMap,
  currentComposition: Composition,
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

export type SolutionDataMap = {
  [key in string]?: SolutionData
};

export type SolutionData = {
  composition: Composition,
  solInfo: {
    solution: Solution,
    loc: Location,
  },
};

export type SaveData = {
  currentActId: ActDataKeys,
  currentLevelId: LevelDataKeys | undefined,
  activeScreen: "menu" | "exec" | "codex" | "settings",
  levelSaves: { [key in LevelDataKeys]?: LevelSaveData },
  animationSpeeds: {
    log: SpeedType,
  },
}

export function emptySaveData(): SaveData {
  return {
    currentActId: 0,
    currentLevelId: undefined,
    activeScreen: "menu",
    levelSaves: {},
    animationSpeeds: {
      log: "play",
    }
  };
}

export function emptyComposition(
  levelId: LevelDataKeys,
): Composition {
  const slots = levelData[levelId].slots;
  return repeat(slots, undefined);
}

export function emptyLevelSaveData(
  levelId: LevelDataKeys,
): LevelSaveData {
  return {
    solMap: {},
    currentComposition: emptyComposition(levelId),
  }
}

export function activeScreen(
  gameRefs: GameRefs,
): ScreenActive {
  switch (gameRefs.saveData.activeScreen) {
    case "menu": {
      return new ScreenAct(gameRefs.saveData.currentActId);
    }
    case "exec": {
      return new ScreenExec(gameRefs.saveData.currentLevelId);
    }
    case "codex": {
      return new ScreenCodex();
    }
    case "settings": {
      return new ScreenSettings();
    }
  }
}

export function selectedActId(
  gameRefs: GameRefs,
): ActDataKeys {
  return gameRefs.saveData.currentActId;
}

export function selectedLevelId(
  gameRefs: GameRefs,
): LevelDataKeys | undefined {
  return gameRefs.saveData.currentLevelId;
}

export function getLevelSaveDataAndFillDefault(
  gameRefs: GameRefs,
  levelId: LevelDataKeys,
): LevelSaveData {
  const levelSaveData = gameRefs.saveData.levelSaves[levelId];
  if (levelSaveData === undefined) {
    // if the level save data is not defined initialize it and return that
    const emptyData = emptyLevelSaveData(levelId);
    gameRefs.saveData.levelSaves[levelId] = emptyData;
    return emptyData;
  } else {
    // if it is defined, just return that
    return levelSaveData;
  }
}

export function getSolutionDataAndFillDefault(
  gameRefs: GameRefs,
  composition: Composition,
) {
  const levelId = selectedLevelId(gameRefs);
  const saveData = getLevelSaveDataAndFillDefault(gameRefs, levelId!);
  const solData = saveData.solMap[compositionToKey(composition)];
  if (solData === undefined) {
    const emptySolData = {
      composition,
      solInfo: {
        solution: emptySolution,
        loc: [],
      }
    };
    saveData.solMap[compositionToKey(composition)] = emptySolData;
    return emptySolData;
  } else {
    return solData;
  }
}

export function validComposition(
  composition: Composition,
  levelId: LevelDataKeys,
) {
  const slots = levelData[levelId].slots;
  return composition.length === slots && composition.findIndex(x => x === undefined) === -1;
}

export function setLocation(
  gameRefs: GameRefs,
  loc: Location,
) {
  const levelId = selectedLevelId(gameRefs);
  if (levelId !== undefined) {
    const saveData = getLevelSaveDataAndFillDefault(gameRefs, levelId);
    const composition = saveData.currentComposition;
    const solInfo = saveData.solMap[compositionToKey(composition)]!.solInfo;
    solInfo.loc = loc;
  } else {
    throw "level id not set!";
  }
}

export function setSolution(
  gameRefs: GameRefs,
  solInfo: { solution: Solution, loc: Location },
) {
  const levelId = selectedLevelId(gameRefs);
  if (levelId !== undefined) {
    const saveData = getLevelSaveDataAndFillDefault(gameRefs, levelId);
    const composition = saveData.currentComposition;
    const solData = getSolutionDataAndFillDefault(gameRefs, composition);
    solData.solInfo = solInfo;
  } else {
    throw "level id not set!";
  }
}

export function currentSolution(
  gameRefs: GameRefs,
) {
  const levelId = selectedLevelId(gameRefs);
  if (levelId !== undefined) {
    const saveData = getLevelSaveDataAndFillDefault(gameRefs, levelId);
    const composition = saveData.currentComposition;
    const solData = getSolutionDataAndFillDefault(gameRefs, composition);
    return solData.solInfo;
  } else {
    throw "level id not set!";
  }
}