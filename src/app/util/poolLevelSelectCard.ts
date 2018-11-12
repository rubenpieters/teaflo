import { Position } from "src/app/util/position";
import { config } from "src/app/config";
import { intersects } from "src/shared/phaser-utils";
import { createPoolHoverCard } from "./poolHoverCard";

export function createPoolLevelSelectCard(
  pool: Phaser.Group,
  slotPool: Phaser.Group,
  hoverViewPool: Phaser.Group,
  pos: Position,
  key: string,
  cardId: string,
): Phaser.Sprite {
  const card: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key);
  card.data.cardId = cardId;
  card.inputEnabled = true;
  card.input.enableDrag(false, true);

  card.events.onInputOver.add(() => {
    const hoverPos: Position = {
      xMin: card.x + config.levelSelectCardWidth + 10,
      xMax: card.x + config.levelSelectCardWidth + 10 + config.hoverCardWidth,
      yMin: card.y,
      yMax: card.y + config.hoverCardHeight,
    };
    card.data.hoverView = createPoolHoverCard(hoverViewPool, hoverPos, key);
  });
  card.events.onInputOut.add(() => {
    if (card.data.hoverView !== undefined) {
      card.data.hoverView.kill();
    }
  });
  
  card.events.onDragStart.removeAll();
  card.events.onDragStart.add(() => {
    if (card.data.hoverView !== undefined) {
      card.data.hoverView.kill();
    }
  });
  card.events.onDragUpdate.removeAll();
  card.events.onDragUpdate.add(() => {
    const cardBounds = card.getBounds();
    let overlap = false;
    slotPool.forEachAlive((slot: Phaser.Sprite) => {
      const slotBounds = slot.getBounds();
      if (! overlap && intersects(cardBounds, slotBounds)) {
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
  
  card.events.onKilled.removeAll();
  card.events.onKilled.add(() => {
    if (card.data.hoverView !== undefined) {
      card.data.hoverView.kill();
    }
    card.data.hoverSlot = undefined;
    card.data.resetSlot = undefined;
    card.data.cardId = undefined;
    card.data.hoverView = undefined;
  });
  card.events.onDestroy.removeAll();
  card.events.onDestroy.add(() => {
    if (card.data.hoverView !== undefined) {
      card.data.hoverView.kill();
    }
    card.data.hoverSlot = undefined;
    card.data.resetSlot = undefined;
    card.data.cardId = undefined;
    card.data.hoverView = undefined;
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