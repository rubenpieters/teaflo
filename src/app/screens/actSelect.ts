import { GameRefs } from "../states/game";
import { createPosition, Position } from "../util/position";
import { config } from "../config";
import { actAvailable } from "../savefile/rep";
import { applyScreenEvent, mkChangeAct } from "../util/screenEvents";
import { actNumberMap } from "../gameData";

export type ActSelectData = {
  btnPool: Phaser.Group,
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
    
    createActSelectButton(game, gameRefs, actNumber, pos, "btn_act",
      () => applyScreenEvent(mkChangeAct(actNumber), game, gameRefs)
    );

    i += 1;
  }
}

export function createActSelectButton(
  game: Phaser.Game,
  gameRefs: GameRefs,
  actNumber: number,
  pos: Position,
  key: string,
  onDownCb: (() => void),
): Phaser.Sprite {
  let frame: number;
  let txtColor: string;
  if (gameRefs.saveFile.activeAct === actNumber) {
    txtColor = "#FF0000",
    frame = DOWN;
  } else if (actAvailable(gameRefs.saveFile, actNumber)) {
    txtColor = "#FF0000",
    frame = NEUTRAL;
  } else {
    txtColor = "#AAAAAA",
    frame = NEUTRAL;
  }
  const btnSprite: Phaser.Sprite = gameRefs.actSelectData.btnPool.getFirstExists(false, true, pos.xMin, pos.yMin, key, frame);
  
  if (btnSprite.data.init === undefined || btnSprite.data.init === false) {
    btnSprite.inputEnabled = true;
    btnSprite.events.onInputDown.add(() => {
      btnSprite.data.selecting = true;
      if (! btnSprite.data.selected) {
        btnSprite.frame = DOWN;
      }
    });
    btnSprite.events.onInputUp.add(() => {
      if (gameRefs.saveFile.activeAct === actNumber) {
        // noop
      } else if (actAvailable(gameRefs.saveFile, actNumber)) {
        onDownCb();
      } else {
        // noop
      }
    });
    btnSprite.events.onInputOver.add(() => {
      if (gameRefs.saveFile.activeAct === actNumber) {
        // noop
      } else if (actAvailable(gameRefs.saveFile, actNumber)) {
        btnSprite.frame = OVER;
      } else {
        // noop
      }
    });
    btnSprite.events.onInputOut.add(() => {
      if (gameRefs.saveFile.activeAct === actNumber) {
        // noop
      } else if (actAvailable(gameRefs.saveFile, actNumber)) {
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
  const btnString = actNumberMap[actNumber];
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