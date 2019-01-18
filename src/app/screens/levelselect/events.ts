import { GameRefs } from "../../states/game";

export function changeLevel(
  gameRefs: GameRefs,
  levelId: string,
) {
  gameRefs.saveData.levelSelect.selectedLevelId = levelId;
}