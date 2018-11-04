export type Position = {
  xMin: number,
  xMax: number,
  yMin: number,
  yMax: number,
}

export function createButton(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
  btnString: string,
  key: string,
  uniqueIn: Phaser.Sprite[],
  onDownCb: (() => void) | undefined,
): Phaser.Sprite {
  const btnSprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key, 0);
  
  return spawnButton(game, btnSprite, pos, btnString, key, uniqueIn, onDownCb);
}

export function spawnButton(
  game: Phaser.Game,
  btnSprite: Phaser.Sprite,
  pos: Position,
  btnString: string,
  key: string,
  uniqueIn: Phaser.Sprite[],
  onDownCb: (() => void) | undefined,
): Phaser.Sprite {
  btnSprite.inputEnabled = true;
  btnSprite.events.onInputUp.removeAll();
  btnSprite.events.onInputUp.add(() => {
    if (! btnSprite.data.selected) {
      // Change this button to selected
      uniqueIn.map(x => {
        if (x.data.selected) {
          x.frame = 0;
        }
        x.data.selected = false;
      });
      btnSprite.frame = 1;
      btnSprite.data.selected = true;
      if (onDownCb !== undefined) {
        onDownCb();
      }
    }
  });
  btnSprite.events.onInputOver.removeAll();
  btnSprite.events.onInputOver.add(() => {
    if (! btnSprite.data.selected) {
      btnSprite.frame = 2;
    }
  });
  btnSprite.events.onInputOut.removeAll();
  btnSprite.events.onInputOut.add(() => {
    if (! btnSprite.data.selected) {
      btnSprite.frame = 0;
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

export function killButton(
  button: Phaser.Sprite,
) {
  button.data.text.destroy();
  button.data.selected = false;
  button.kill();
}