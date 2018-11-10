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
  card.data.cardId = cardId;
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
        // clear drop slot
        card.data.dropSlot.data.card = undefined;
        // set the drop slot of the card to its hover slot
        card.data.dropSlot = card.data.hoverSlot;
      } else {
        console.log("SWAP");
        // the hover slot does contain a card
        // aliases
        const replacedCard = card.data.hoverSlot.data.card;
        // swap it with the currently dropped card
        // - first replaced card to drop slot
        card.data.dropSlot.data.card = replacedCard;
        replacedCard.data.dropSlot = card.data.hoverSlot;
        // - then this card to hover slot
        card.data.hoverSlot.data.card = card;
        card.data.dropSlot = card.data.hoverSlot;
        // move replaced card
        replacedCard.x = card.data.originalPos.x;
        replacedCard.y = card.data.originalPos.y;
      }
      card.data.originalPos = card.data.dropPos;
    }
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      slot.frame = 0;
    });
  });

  return card;
}
