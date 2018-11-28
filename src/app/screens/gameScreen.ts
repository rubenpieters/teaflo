import { GameRefs } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";
import { mkGoToMenu, applyScreenEvent } from "../util/screenEvents";
import { Button } from "../util/btn";
import { SpritePool } from "../util/pool";

export type GameScreenData = {
  spriteGroup: Phaser.Group,
  unitPool: Phaser.Group,
  unitHpPool: Phaser.Group,
  logBtnPool: SpritePool<Button>,
  unitAbilityPool: Phaser.Group,
  solTreePool: Phaser.Graphics[],
  exitBtn?: Phaser.Sprite,
  victoryBtn?: Phaser.Sprite,
}

export function drawGameScreen(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  if (gameRefs.gameScreenData.exitBtn === undefined) {
    const exitBtnPos = createPosition(
      "right", 250, config.levelButtonWidth,
      "top", 400, config.levelButtonHeight,
    );
    const exitBtn = createButton(game, gameRefs.gameScreenData.spriteGroup, exitBtnPos, "Exit", "btn_level",
      () => {
        applyScreenEvent(mkGoToMenu(exitBtn.data.levelId), game, gameRefs)
      }
    );
    gameRefs.gameScreenData.exitBtn = exitBtn;
  }
  gameRefs.gameScreenData.exitBtn.data.levelId = levelId;
}