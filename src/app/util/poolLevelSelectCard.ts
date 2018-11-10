import { Position } from "src/app/util/position";
import { config } from "../config";

export function createPoolLevelSelectCard(
  game: Phaser.Game,
  pool: Phaser.Group,
  slotPool: Phaser.Group,
  pos: Position,
  key: string,
  cardId: string,
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
        card.data.hoverSlot = slot;
        overlap = true;
      } else {
        slot.frame = 0;
      }
    });
    if (! overlap) {
      card.data.dropPos = undefined;
      card.data.hoverSlot = undefined;
    }
  });
  card.events.onDragStop.removeAll();
  card.events.onDragStop.add(() => {
    if (card.data.dropPos === undefined) {
      card.x = card.data.originalPos.x;
      card.y = card.data.originalPos.y;
      card.data.dropSlot.data.card = card;
    } else {
      card.x = card.data.dropPos.x;
      card.y = card.data.dropPos.y;
      if (card.data.hoverSlot.data.card === undefined) {
        console.log("PLACE");
        // the hover slot does not contain a card
        // just place it there
        card.data.hoverSlot.data.card = card;
        console.log(`hover: ${card.data.hoverSlot.data.card.cardId}`);
        // clear drop slot
        if (card.data.dropSlot !== undefined) {
          card.data.dropSlot.data.card = undefined;
        }
      } else {
        console.log("SWAP");
        // the hover slot does contain a card
        // swap it with the currently dropped card
        card.data.dropSlot.data.card = card.data.hoverSlot.data.card;
        console.log(`drop: ${card.data.dropSlot.data.card.cardId}`);
        card.data.hoverSlot.data.card = card;
        console.log(`hover: ${card.data.hoverSlot.data.card.cardId}`);
        // move hover slot card (now in drop slot)
        card.data.dropSlot.data.card.x = card.data.originalPos.x;
        card.data.dropSlot.data.card.y = card.data.originalPos.y;
      }
      card.data.originalPos = card.data.dropPos;
      card.data.dropSlot = card.data.hoverSlot;
    }
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      slot.frame = 0;
    });
  });

  return card;
}
