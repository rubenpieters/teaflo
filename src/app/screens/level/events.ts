import { GameRefs } from "../../states/game";


export function loadLevel(
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {

  gameRefs.screens.actScreen.setVisibility(false);
  gameRefs.screens.levelScreen.setVisibility(true);

  gameRefs.screens.levelScreen.drawBox();
}