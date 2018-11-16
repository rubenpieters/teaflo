import { GameRefs } from "../states/game";
import { levelMap } from "../gameData";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelAvailable } from "../savefile/rep";
import { applyScreenEvent, mkChangeLevel } from "../util/screenEvents";

export type LevelSelectData = {
  btnPool: Phaser.Group,
}

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function drawLevelSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
  act: number,
) {
  gameRefs.levelSelectData.btnPool.killAll();

  let i = 0;
  for (const levelId of levelMap[act]) {  
    const pos = createPosition(
      "left", 250, config.levelButtonWidth,
      "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
    );

    createLevelSelectButton(game, gameRefs, levelId, pos, "btn_level",
      () => applyScreenEvent(mkChangeLevel(levelId), game, gameRefs)
    );

    i += 1;
  }
}

export function createLevelSelectButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
  pos: Position,
  key: string,
  onDownCb: (() => void),
): Phaser.Sprite {
  let frame: number;
  let txtColor: string;
  if (gameRefs.saveFile.activeLevel === levelId) {
    txtColor = "#FF0000",
    frame = DOWN;
  } else if (levelAvailable(gameRefs.saveFile, levelId)) {
    txtColor = "#FF0000",
    frame = NEUTRAL;
  } else {
    txtColor = "#AAAAAA",
    frame = NEUTRAL;
  }
  const btnSprite: Phaser.Sprite = gameRefs.levelSelectData.btnPool.getFirstExists(false, true, pos.xMin, pos.yMin, key, frame);
  
  btnSprite.data.selecting = false;
  btnSprite.data.levelId = levelId;
  btnSprite.data.onDownCb = onDownCb;

  if (btnSprite.data.init === undefined || btnSprite.data.init === false) {
    btnSprite.inputEnabled = true;
    btnSprite.events.onInputDown.add(() => {
      btnSprite.data.selecting = true;
      if (gameRefs.saveFile.activeLevel === btnSprite.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btnSprite.data.levelId)) {
        btnSprite.frame = DOWN;
      } else {
        // noop
      }
    });
    btnSprite.events.onInputUp.add(() => {
      if (
        inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
      ) {
        if (gameRefs.saveFile.activeLevel === btnSprite.data.levelId) {
          // noop
        } else if (levelAvailable(gameRefs.saveFile, btnSprite.data.levelId)) {
          btnSprite.data.onDownCb();
        } else {
          // noop
        }
      }
    });
    btnSprite.events.onInputOver.add(() => {
      if (gameRefs.saveFile.activeLevel === btnSprite.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btnSprite.data.levelId)) {
        if (btnSprite.data.selecting) {
          btnSprite.frame = DOWN;
        } else {
          btnSprite.frame = OVER;
        }
      } else {
        // noop
      }
    });
    btnSprite.events.onInputOut.add(() => {
      if (gameRefs.saveFile.activeLevel === btnSprite.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btnSprite.data.levelId)) {
        btnSprite.frame = NEUTRAL;
      } else {
        // noop
      }
    });
  
    btnSprite.events.onKilled.add(() => {
      btnSprite.data.btnText.destroy();
    });
    btnSprite.events.onDestroy.add(() => {
      btnSprite.data.btnText.destroy();
    });

    btnSprite.data.init = true;
  }
  const btnString = levelId;
  const btnText = game.add.text(
    0, 0, btnString, {
      fill: txtColor,
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
  btnSprite.addChild(btnText);
  btnSprite.data.btnText = btnText;

  return btnSprite;
}