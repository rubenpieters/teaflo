import { GameRefs } from "../../states/game";
import { SelectedBuildSchem } from "../act/data";


export function loadLevel(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {
  gameRefs.saveData.act.currentSchem = new SelectedBuildSchem(levelId, solId);

  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.levelScreen.setVisibility(true);

  gameRefs.screens.levelScreen.drawBox();
}