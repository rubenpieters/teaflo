import { GameRefs } from "src/app/states/game";
import { config } from "../config";
import { createButton } from "../util/button";
import { createPosition } from "../util/position";
import { GameState } from "src/shared/game/gameState";

export type GameScreenData = {
  spriteGroup: Phaser.Group,
  unitPool: Phaser.Group,
  exitBtn?: Phaser.Sprite,
  victoryBtn?: Phaser.Sprite,
}

export function gameScreen_Main(
  game: Phaser.Game,
  gameRefs: GameRefs,
  gameState: GameState,
  levelId: string,
) {
  console.log(JSON.stringify(gameState));
  if (gameRefs.gameScreenData.exitBtn === undefined) {
    const exitBtnPos = createPosition(
      "right", 250, config.levelButtonWidth,
      "top", 400, config.levelButtonHeight,
    );
    const exitBtn = createButton(game, gameRefs.gameScreenData.spriteGroup, exitBtnPos, "Exit", "btn_level",
      () => console.log("TODO") // gameScreenToLevelSelect(game, undefined)
    );
    gameRefs.gameScreenData.exitBtn = exitBtn;
  }

  if (gameRefs.gameScreenData.victoryBtn !== undefined) {
    gameRefs.gameScreenData.victoryBtn.destroy();
  }
  const victoryBtnPos = createPosition(
    "right", 700, config.levelButtonWidth,
    "top", 400, config.levelButtonHeight,
  );
  const victoryBtn = createButton(game, gameRefs.gameScreenData.spriteGroup, victoryBtnPos, "Claim", "btn_level",
    () => console.log("TODO") // gameScreenToLevelSelect(game, levelId)
  );
  gameRefs.gameScreenData.victoryBtn = victoryBtn;
}