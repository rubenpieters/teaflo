import { GameScreen, gameScreenToLevelSelect } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";

export function gameScreen_Main(
  game: Phaser.Game,
  cards: string[],
  gameScreen: GameScreen,
) {
  console.log(JSON.stringify(cards));
  if (gameScreen.exitBtn === undefined) {
    const exitBtnPos = createPosition(
      "right", 250, config.levelButtonWidth,
      "top", 400, config.levelButtonHeight,
    );
    const exitBtn = createButton(game, gameScreen.group, exitBtnPos, "Exit", "btn_level",
      () => gameScreenToLevelSelect(game)
    );
    gameScreen.exitBtn = exitBtn;
  }
}