import { GameRefs, LevelSelect } from "../states/game";
import { createPosition } from "../util/position";
import { config } from "../config";
import { actAvailable } from "../savefile/rep";
import { createPoolButton } from "../util/poolButton";
import { levelSelect_Main } from "./general";
import { createLockedPoolButton } from "../util/lockedPoolButton";
import { applyScreenEvent, mkChangeLevel, mkChangeAct } from "../util/screenEvents";
import { actNumberMap } from "../gameData";

export function drawActSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
) {
  // draw each act select button
  gameRefs.actSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );
    
    let button: Phaser.Sprite;
    if (actAvailable(gameRefs.saveFile, Number(actNumber))) {
      button = createPoolButton(game, gameRefs.actSelectBtnPool, pos, btnString, "btn_act",
        () => applyScreenEvent(mkChangeAct(Number(actNumber)), game, gameRefs)
      );
    } else {
      button = createLockedPoolButton(game, gameRefs.actSelectBtnPool, pos, btnString, "btn_act");
    }

    i += 1;
  }
}