import { Position, inPosition } from "src/app/util/position";

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function createPoolButton(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
  btnString: string,
  key: string,
  onDownCb: (() => void) | undefined,
): Phaser.Sprite {
  const btnSprite: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key, NEUTRAL);
  
  btnSprite.inputEnabled = true;
  btnSprite.events.onInputDown.removeAll();
  btnSprite.events.onInputDown.add(() => {
    btnSprite.data.selecting = true;
    if (! btnSprite.data.selected) {
      btnSprite.frame = DOWN;
    }
  });
  btnSprite.events.onInputUp.removeAll();
  btnSprite.events.onInputUp.add((opts: { force: boolean }) => {
    btnSprite.data.selecting = false;
    if (
      opts.force ||
      inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
    ) {
      if (! btnSprite.data.selected) {
        // Unselect other buttons
        pool.forEachAlive((x: Phaser.Sprite) => {
          if (x.data.selected) {
            x.frame = NEUTRAL;
          }
          x.data.selected = false;
        });
        // Change this button to selected
        btnSprite.data.selected = true;
        if (onDownCb !== undefined) {
          onDownCb();
        }
      }
      
      btnSprite.frame = DOWN;
    }
  });
  btnSprite.events.onInputOver.removeAll();
  btnSprite.events.onInputOver.add(() => {
    if (btnSprite.data.selecting || btnSprite.data.selected) {
      btnSprite.frame = DOWN;
    } else {
      btnSprite.frame = OVER;
    }
  });
  btnSprite.events.onInputOut.removeAll();
  btnSprite.events.onInputOut.add(() => {
    if (btnSprite.data.selected) {
      btnSprite.frame = DOWN;
    } else {
      btnSprite.frame = NEUTRAL;
    }
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
  btnSprite.data.text = btnText;

  return btnSprite;
}

export function killPoolButton(
  button: Phaser.Sprite,
) {
  button.data.text.destroy();
  button.data.selected = false;
  button.data.selecting = false;
  button.kill();
}