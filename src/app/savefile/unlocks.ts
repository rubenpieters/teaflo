import { focus, over, set } from "src/shared/iassign-util";
import { GameRefs } from "../states/game";

export type Unlocks = {
  levels: string[],
  acts: number[],
}

export function applyUnlocks(
  gameRefs: GameRefs,
) {
  // a1_l2
  if (
    gameRefs.saveFile.levelSolutions["a1_l1"] !== undefined &&
    gameRefs.saveFile.levelSolutions["a1_l1"][gameRefs.saveFile.activeSolutions["a1_l1"]].solution.win
  ) {
    gameRefs.saveFile.levelUnlocked["a1_l2"] = "unlocked";
  }
  // act 2
  if (
    gameRefs.saveFile.levelSolutions["a1_l2"] !== undefined &&
    gameRefs.saveFile.levelSolutions["a1_l2"][gameRefs.saveFile.activeSolutions["a1_l2"]].solution.win
  ) {
    gameRefs.saveFile.actUnlocked[1] = "unlocked";
    gameRefs.saveFile.levelUnlocked["a2_l1"] = "unlocked";
    gameRefs.saveFile.levelUnlocked["a2_l2"] = "unlocked";
    gameRefs.saveFile.levelUnlocked["a2_l3"] = "unlocked";
  }
}
