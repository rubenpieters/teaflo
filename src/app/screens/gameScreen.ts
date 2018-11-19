import { GameRefs } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";
import { mkGoToMenu, applyScreenEvent } from "../util/screenEvents";

export type GameScreenData = {
  spriteGroup: Phaser.Group,
  unitPool: Phaser.Group,
  exitBtn?: Phaser.Sprite,
  victoryBtn?: Phaser.Sprite,
}

export function drawGameScreen(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  console.log(JSON.stringify(gameRefs.saveFile.levelSolutions[levelId][gameRefs.saveFile.activeSolutions[levelId]]));
  if (gameRefs.gameScreenData.exitBtn === undefined) {
    const exitBtnPos = createPosition(
      "right", 250, config.levelButtonWidth,
      "top", 400, config.levelButtonHeight,
    );
    const exitBtn = createButton(game, gameRefs.gameScreenData.spriteGroup, exitBtnPos, "Exit", "btn_level",
      () => applyScreenEvent(mkGoToMenu(), game, gameRefs)
    );
    gameRefs.gameScreenData.exitBtn = exitBtn;
  }
}