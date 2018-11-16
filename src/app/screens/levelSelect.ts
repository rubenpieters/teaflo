import { GameRefs, LevelSelect } from "../states/game";
import { levelMap } from "../gameData";
import { createPosition } from "../util/position";
import { config } from "../config";
import { levelAvailable } from "../savefile/rep";
import { createPoolButton } from "../util/poolButton";
import { createLockedPoolButton } from "../util/lockedPoolButton";
import { applyScreenEvent, mkChangeLevel } from "../util/screenEvents";

export function drawLevelSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
  act: number,
) {
  gameRefs.levelSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  let i = 0;
  for (const levelId of levelMap[act]) {
    let btnString = levelId;
  
    const pos = createPosition(
      "left", 250, config.levelButtonWidth,
      "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
    );

    let button: Phaser.Sprite;
    if (levelAvailable(gameRefs.saveFile, levelId)) {
      button = createPoolButton(game, gameRefs.levelSelectBtnPool, pos, btnString, "btn_level",
        () => applyScreenEvent(mkChangeLevel(levelId), game, gameRefs)
      );
    } else {
      button = createLockedPoolButton(game, gameRefs.levelSelectBtnPool, pos, btnString, "btn_level");
    }

    i += 1;
  }
}