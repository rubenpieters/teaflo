import { Position } from "src/app/util/position";

export function createPoolCardSlot(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
): Phaser.Sprite {
  const card: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, "card_slot", 0);

  return card;
}