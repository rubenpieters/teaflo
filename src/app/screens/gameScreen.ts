import { GameRefs } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";
import { mkGoToMenu, applyScreenEvent } from "../util/screenEvents";
import { Button } from "../util/btn";
import { SpritePool } from "../util/pool";
import { ClickState } from "../util/clickState";

export type GameScreenData = {
  spriteGroup: Phaser.Group,
  unitPool: SpritePool<Button>,
  unitHpPool: Phaser.Group,
  logBtnPool: SpritePool<Button>,
  unitAbilityPool: Phaser.Group,
  solTreePool: Phaser.Graphics[],
  exitBtn?: Phaser.Sprite,
  victoryBtn?: Phaser.Sprite,
  clickState?: ClickState,
  levelId: string,
}

export function drawGameScreen(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
) {
  if (gameRefs.gameScreenData.exitBtn === undefined) {
    const exitBtnPos = createPosition(
      "right", 250, config.levelButtonWidth,
      "bot", 250, config.levelButtonHeight,
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