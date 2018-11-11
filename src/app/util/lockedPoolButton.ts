import { Position } from "src/app/util/position";

const NEUTRAL = 0;
//const DOWN = 1;
//const OVER = 2;

export function createLockedPoolButton(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
  btnString: string,
  key: string,
): Phaser.Sprite {
  const btnSprite: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key, NEUTRAL);

  const btnText = game.add.text(
    0, 0, `${btnString}`, {
      fill: "#AAAAAA",
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
    btnSprite.data.selected = false;
    btnSprite.data.selecting = false;
  });
  btnSprite.events.onDestroy.removeAll();
  btnSprite.events.onDestroy.add(() => {
    btnText.destroy();
    btnSprite.data.selected = false;
    btnSprite.data.selecting = false;
  });

  return btnSprite;
}