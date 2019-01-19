import { GameRefs } from "../../states/game";
import { selectedLevelId, levelMap } from "../levelselect/data";

export function changeAct(
  gameRefs: GameRefs,
  actId: number,
) {
  gameRefs.saveData.act.selectedActId = actId;
  gameRefs.screens.actScreen.redraw();
  const levelId = selectedLevelId(gameRefs, actId)
  if (levelId === undefined) {
    gameRefs.saveData.levelSelect.selectedLevelId[actId] = levelMap[actId][0].id;
  }
  gameRefs.screens.levelSelectScreen.draw(actId);
}
