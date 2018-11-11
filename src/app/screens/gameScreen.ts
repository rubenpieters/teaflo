import { GameScreen, gameScreenToLevelSelect } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";
import { GameState } from "src/shared/game/gameState";

export function gameScreen_Main(
  game: Phaser.Game,
  gameState: GameState,
  gameScreen: GameScreen,
) {
  console.log(JSON.stringify(gameState));
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

  if (gameScreen.victoryBtn === undefined) {
    const victoryBtnPos = createPosition(
      "right", 700, config.levelButtonWidth,
      "top", 400, config.levelButtonHeight,
    );
    const victoryBtn = createButton(game, gameScreen.group, victoryBtnPos, "Claim", "btn_level",
      () => gameScreenToLevelSelect(game)
    );
    gameScreen.victoryBtn = victoryBtn;
  }
}