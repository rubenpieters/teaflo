import { focus, over, set } from "src/shared/iassign-util";
import { SaveFileV1 } from "./rep";

export type Unlocks = {
  levels: string[],
  acts: number[],
}

export function applyUnlocks(
  saveFile: SaveFileV1,
  levelId: string,
): SaveFileV1 {
  const unlocks = unlockOrder(levelId);
  let newSaveFile = saveFile;
  for (const unlockLevelId of unlocks.levels) {
    newSaveFile = focus(newSaveFile, set(x => x.levelUnlocked[unlockLevelId], "unlocked"));
  }
  for (const unlockAct of unlocks.acts) {
    newSaveFile = focus(newSaveFile, set(x => x.actUnlocked[unlockAct], "unlocked"));
  }
  return newSaveFile
}

export function unlockOrder(
  levelId: string,
) {
  switch (levelId) {
    case "a1_l1": {
      return {
        levels: ["a1_l2"],
        acts: [],
      };
    }
    case "a1_l2": {
      return {
        levels: ["a2_l1", "a2_l2", "a2_l3"],
        acts: [1],
      }
    }
    default:
      throw `unimplemented level id: ${levelId}`;
  }
}