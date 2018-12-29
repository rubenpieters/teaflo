import { GameRefs } from "../states/game";
import { createPosition, Position, inPosition } from "../util/position";
import { config } from "../config";
import { actAvailable } from "../savefile/rep";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { actNumberMap } from "../gameData";
import { GSprite } from "src/shared/phaser-util";
import { createButtonInPool, Button, addText, ButtonValues } from "../util/btn";
import { SpritePool } from "../util/pool";

export type ActSelectData = {
  btnPool: SpritePool<ActSelectButton>,
}

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function drawActSelect(
  game: Phaser.Game,
  gameRefs: GameRefs,
) {
  // draw each act select button
  gameRefs.actSelectData.btnPool.killAll();

  let i = 0;
  for (const actKey in actNumberMap) {
    const actNumber = Number(actKey);

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );
    
    createActSelectButton(game, gameRefs, actNumber, pos, "btn_act");

    i += 1;
  }
}

type ActSelectButton = GSprite<ButtonValues & {
  actNumber: number,
  btnText: Phaser.Text,
}>;

export function createActSelectButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  actNumber: number,
  pos: Position,
  key: string,
): ActSelectButton {
  let frame: number;
  let txtColor: string;
  if (gameRefs.saveFile.activeAct === actNumber) {
    txtColor = "#FF0000";
    frame = DOWN;
  } else if (actAvailable(gameRefs.saveFile, actNumber)) {
    txtColor = "#FF0000";
    frame = NEUTRAL;
  } else {
    txtColor = "#AAAAAA";
    frame = NEUTRAL;
  }
  
  const btn = createButtonInPool(
    game,
    gameRefs.actSelectData.btnPool,
    pos,
    { actNumber },
    key,
    {
      onDown: () => {
        if (gameRefs.saveFile.activeAct === btn.data.actNumber) {
          // noop
        } else if (actAvailable(gameRefs.saveFile, btn.data.actNumber)) {
          btn.frame = DOWN;
        } else {
          // noop
        }
      },
      clickLeft: () => {
        if (gameRefs.saveFile.activeAct === btn.data.actNumber) {
          // noop
        } else if (actAvailable(gameRefs.saveFile, btn.data.actNumber)) {
          applyScreenEvent(new SE.ChangeAct(btn.data.actNumber), game, gameRefs)
        } else {
          // noop
        }
      },
      hoverOver: () => {
        if (gameRefs.saveFile.activeAct === btn.data.actNumber) {
          // noop
        } else if (actAvailable(gameRefs.saveFile, btn.data.actNumber)) {
          if (btn.data.selectingStatus !== "none") {
            btn.frame = DOWN;
          } else {
            btn.frame = OVER;
          }
        } else {
          // noop
        }
      },
      hoverOut: () => {
        if (gameRefs.saveFile.activeAct === btn.data.actNumber) {
          // noop
        } else if (actAvailable(gameRefs.saveFile, btn.data.actNumber)) {
          btn.frame = NEUTRAL;
        } else {
          // noop
        }
      },
    },
    frame,
  );

  const btnString = actNumberMap[actNumber];
  return addText(game, btn, pos, btnString, txtColor, 100);
}