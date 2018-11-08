import { Position } from "src/app/util/position";
import { config } from "../config";

export function createPoolLevelSelectCard(
  game: Phaser.Game,
  pool: Phaser.Group,
  slotPool: Phaser.Group,
  pos: Position,
  key: string,
): Phaser.Sprite {
  const card: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  card.data.originalPos = { x: pos.xMin, y: pos.yMin };
  card.inputEnabled = true;
  card.input.enableDrag(false, true);
  
  card.events.onDragUpdate.removeAll();
  card.events.onDragUpdate.add(() => {
    const cardBounds = card.getBounds();
    let overlap = false;
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      const slotBounds = slot.getBounds();
      if (! overlap && Phaser.Rectangle.intersects(<any>cardBounds, <any>slotBounds)) {
        slot.frame = 1;
        card.data.dropPos = { x: slot.x - (2 * config.levelBgWidth / 100), y: slot.y - (2 * config.levelBgHeight / 100) };
        overlap = true;
      } else {
        slot.frame = 0;
      }
    });
  });
  card.events.onDragStop.removeAll();
  card.events.onDragStop.add(() => {
    if (card.data.dropPos === undefined) {
      card.x = card.data.originalPos.x;
      card.y = card.data.originalPos.y;
    } else {
      card.x = card.data.dropPos.x;
      card.y = card.data.dropPos.y;
    }
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      slot.frame = 0;
    });
  });
  

  return card;
}
