import { Position } from "src/app/util/position";

export function createPoolLevelSelectCard(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
  key: string,
): Phaser.Sprite {
  const card: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  card.inputEnabled = true;
  card.input.enableDrag();


  return card;
}