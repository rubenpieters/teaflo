import { Position, inPosition } from "src/app/util/position";

export const NEUTRAL = 0;
export const DOWN = 1;
export const OVER = 2;

export function createButton(
  game: Phaser.Game,
  group: Phaser.Group,
  pos: Position,
  btnString: string,
  key: string,
  onDownCb: (() => void) | undefined,
): Phaser.Sprite {
  const btnSprite: Phaser.Sprite = game.add.sprite(pos.xMin, pos.yMin, key, NEUTRAL, group);
  
  btnSprite.inputEnabled = true;
  btnSprite.events.onInputDown.removeAll();
  btnSprite.events.onInputDown.add(() => {
    btnSprite.data.selecting = true;
    btnSprite.frame = DOWN;
  });
  btnSprite.events.onInputUp.removeAll();
  btnSprite.events.onInputUp.add((opts: { force: boolean }) => {
    btnSprite.data.selecting = false;
    if (
      opts.force ||
      inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
    ) {
      if (onDownCb !== undefined) {
        onDownCb();
      }

      btnSprite.frame = NEUTRAL;
    }
  });
  btnSprite.events.onInputOver.removeAll();
  btnSprite.events.onInputOver.add(() => {
    if (btnSprite.data.selecting) {
      btnSprite.frame = DOWN;
    } else {
      btnSprite.frame = OVER;
    }
  });
  btnSprite.events.onInputOut.removeAll();
  btnSprite.events.onInputOut.add(() => {
    btnSprite.frame = NEUTRAL;
  });
  const btnText = game.add.text(
    0, 0, btnString, {
      fill: "#FF0000",
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }
  );
  btnText.setTextBounds(0, 0, pos.xMax - pos.xMin, pos.yMax - pos.yMin);
  btnSprite.addChild(btnText);

  btnSprite.events.onKilled.removeAll();
  btnSprite.events.onKilled.add(() => {
    btnText.destroy();
    btnSprite.data.selecting = false;
  });
  btnSprite.events.onDestroy.removeAll();
  btnSprite.events.onDestroy.add(() => {
    btnText.destroy();
    btnSprite.data.selecting = false;
  });

  return btnSprite;
}