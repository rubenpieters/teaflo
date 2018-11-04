import { Position, inPosition } from "src/app/util/position";

const NEUTRAL = 0;
const DOWN = 1;
const OVER = 2;

export function createButton(
  game: Phaser.Game,
  pos: Position,
  btnString: string,
  key: string,
  onDownCb: (() => void) | undefined,
): Phaser.Sprite {
  const btnSprite: Phaser.Sprite = game.add.sprite(pos.xMin, pos.yMin, key, NEUTRAL);
  
  btnSprite.inputEnabled = true;
  btnSprite.events.onInputDown.add(() => {
    btnSprite.data.selecting = true;
    btnSprite.frame = DOWN;
  });
  btnSprite.events.onInputUp.add((opts: { force: boolean }) => {
    btnSprite.data.selecting = false;
    if (
      opts.force ||
      inPosition(pos, game.input.activePointer.x, game.input.activePointer.y)
    ) {
      if (! btnSprite.data.selected) {
        // Change this button to selected
        btnSprite.data.selected = true;
        if (onDownCb !== undefined) {
          onDownCb();
        }
      }
      
      btnSprite.frame = DOWN;
    }
  });
  btnSprite.events.onInputOver.add(() => {
    if (btnSprite.data.selecting) {
      btnSprite.frame = DOWN;
    } else {
      btnSprite.frame = OVER;
    }
  });
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
  btnSprite.data.text = btnText;

  return btnSprite;
}

export function killButton(
  button: Phaser.Sprite,
) {
  button.data.text.destroy();
  button.data.selecting = false;
  button.kill();
}