import { GameRefs } from "../states/game";
import { levelMap } from "../gameData";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { levelAvailable } from "../savefile/rep";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { GSprite } from "src/shared/phaser-util";
import { createButtonInPool, addText, ButtonValues } from "../util/btn";
import { SpritePool } from "../util/pool";

export type LevelSelectData = {
  btnPool: SpritePool<LevelSelectButton>,
  cardSlotPool: Phaser.Group,
  cardPool: Phaser.Group,
  solBtnPool: Phaser.Group,
  spriteGroup: Phaser.Group,
  rightBg?: Phaser.Sprite,
  leftBg?: Phaser.Sprite,
  startBtn?: Phaser.Sprite,
  addSolBtn?: Phaser.Sprite,
}

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function drawLevelSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
  act: number,
  animation: boolean,
) {
  gameRefs.levelSelectData.btnPool.killAll();

  let i = 0;
  for (const levelId of levelMap[act]) {  
    const pos = createPosition(
      "left", 250, config.levelButtonWidth,
      "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
    );

    const button = createLevelSelectButton(game, gameRefs, levelId, pos, "btn_level");

    if (animation) {
      button.x = pos.xMin - 200 + (30 * i);
      const introAnim = game.add.tween(button);
      introAnim.frameBased = true;
      introAnim.to({ x: pos.xMin }, 26 - (4 * i), Phaser.Easing.Linear.None, true, 50);
    }

    i += 1;
  }
}

type LevelSelectButton = GSprite<ButtonValues & {
  levelId: string,
  btnText: Phaser.Text,
}>;

export function createLevelSelectButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
  pos: Position,
  key: string,
): LevelSelectButton {
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
  
  const btn = createButtonInPool(
    game,
    gameRefs.levelSelectData.btnPool,
    pos,
    { levelId },
    key,
    frame,
    // onInputDown
    () => {
      if (gameRefs.saveFile.activeLevel === btn.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btn.data.levelId)) {
        btn.frame = DOWN;
      } else {
        // noop
      }
    },
    // onInputUp
    () => {
      if (gameRefs.saveFile.activeLevel === btn.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btn.data.levelId)) {
        applyScreenEvent(new SE.ChangeLevel(btn.data.levelId, false), game, gameRefs);
      } else {
        // noop
      }
    },
    // onInputOver
    () => {
      if (gameRefs.saveFile.activeLevel === btn.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btn.data.levelId)) {
        if (btn.data.selectingStatus !== "none") {
          btn.frame = DOWN;
        } else {
          btn.frame = OVER;
        }
      } else {
        // noop
      }
    },
    // onInputOut
    () => {
      if (gameRefs.saveFile.activeLevel === btn.data.levelId) {
        // noop
      } else if (levelAvailable(gameRefs.saveFile, btn.data.levelId)) {
        btn.frame = NEUTRAL;
      } else {
        // noop
      }
    },
  );

  const btnString = levelId;
  return addText(game, btn, pos, btnString, txtColor, 100);
}