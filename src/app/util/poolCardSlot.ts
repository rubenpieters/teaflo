import { Position } from "src/app/util/position";

export function createPoolCardSlot(
  game: Phaser.Game,
  pool: Phaser.Group,
  pos: Position,
): Phaser.Sprite {
  const slot: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, "card_slot", 0);
  
  slot.events.onKilled.removeAll();
  slot.events.onKilled.add(() => {
    slot.data.choice = undefined;
  });
  slot.events.onDestroy.removeAll();
  slot.events.onDestroy.add(() => {
    slot.data.choice = undefined;
  });

  return slot;
}