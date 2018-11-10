import { Position } from "src/app/util/position";
import { config } from "../config";

export function createPoolLevelSelectCard(
  pool: Phaser.Group,
  slotPool: Phaser.Group,
  pos: Position,
  key: string,
  cardId: string,
): Phaser.Sprite {
  const card: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
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
        card.data.hoverSlot = slot;
        overlap = true;
      } else {
        slot.frame = 0;
      }
    });
    if (! overlap) {
      card.data.hoverSlot = undefined;
    }
  });
  card.events.onDragStop.removeAll();
  card.events.onDragStop.add(() => {
    if (card.data.hoverSlot === undefined) {
      card.data.resetSlot.data.card = card;
      moveToSlot(card, card.data.resetSlot);
    } else {
      moveToSlot(card, card.data.hoverSlot);
      if (card.data.hoverSlot.data.card === undefined) {
        // the hover slot does not contain a card
        // just place it there
        card.data.hoverSlot.data.card = card;
        // reset the card info of its reset slot
        card.data.resetSlot.data.card = undefined;
        // its reset slot is now the hover slot
        card.data.resetSlot = card.data.hoverSlot;
      } else {
        // the hover slot does contain a card
        // alias
        const replacedCard = card.data.hoverSlot.data.card;
        // swap it with the currently dropped card
        // - first replaced card to drop slot
        card.data.resetSlot.data.card = replacedCard;
        replacedCard.data.resetSlot = card.data.resetSlot;
        // - then this card to hover slot
        card.data.hoverSlot.data.card = card;
        card.data.resetSlot = card.data.hoverSlot;
        // move replaced card
        moveToSlot(replacedCard, replacedCard.data.resetSlot);
      }
    }
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      slot.frame = 0;
    });
  });

  return card;
}

function moveToSlot(
  card: Phaser.Sprite,
  slot: Phaser.Sprite,
) {
  card.x = slot.x - (2 * config.levelBgWidth / 100);
  card.y = slot.y - (2 * config.levelBgHeight / 100);
}