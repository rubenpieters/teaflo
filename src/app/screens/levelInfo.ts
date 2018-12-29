import { GameRefs } from "../states/game";
import { createPosition, absoluteIn, Position } from "../util/position";
import { config } from "../config";
import { createButton } from "../util/button";
import { levelDataMap } from "../gameData";
import { createPoolCardSlot } from "../util/poolCardSlot";
import { intersects, GSprite } from "../../shared/phaser-util";
import { applyScreenEvent } from "../util/screenEvents";
import * as SE from "../util/screenEvents";
import { TargetType } from "../../shared/game/entityId";
import { spriteMap } from "../../shared/data/units/spriteMap";
import { createButtonInPool, ButtonValues } from "../util/btn";

export function drawLevelInfo(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelId: string,
  solId: number,
) {
  gameRefs.levelSelectData.cardSlotPool.killAll();
  gameRefs.levelSelectData.cardPool.killAll();

  // left background
  const leftBgSpritePos = createPosition(
    "right", 1650, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (gameRefs.levelSelectData.leftBg === undefined) {
    const leftBgSprite = game.add.sprite(leftBgSpritePos.xMin, leftBgSpritePos.yMin, "bg_level", undefined, gameRefs.levelSelectData.spriteGroup);
    gameRefs.levelSelectData.leftBg = leftBgSprite;
  }

  // right background
  const rightBgSpritePos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (gameRefs.levelSelectData.rightBg === undefined) {
    const rightBgSprite = game.add.sprite(rightBgSpritePos.xMin, rightBgSpritePos.yMin, "bg_level", undefined, gameRefs.levelSelectData.spriteGroup);
    gameRefs.levelSelectData.rightBg = rightBgSprite;
  }

  // start game button
  if (gameRefs.levelSelectData.startBtn !== undefined) {
    gameRefs.levelSelectData.startBtn.destroy();
  }
  const startBtnPos = absoluteIn(
    rightBgSpritePos, config.levelBgWidth, config.levelBgHeight,
    70, config.levelButtonWidth,
    90, config.levelButtonHeight,
  );
  const startBtn = createButton(game, gameRefs.levelSelectData.spriteGroup, startBtnPos, "Start", "btn_level",
    () => {
      applyScreenEvent(new SE.StartLevel(), game, gameRefs);
    }
  );
  gameRefs.levelSelectData.startBtn = startBtn;

  const supplyPool: (string | undefined)[] = levelDataMap[levelId].cardIds.concat();
  const supplySize = supplyPool.length;

  // solution card slots
  for (let i = 0; i < levelDataMap[levelId].slots; i++) {
    const cardSlotPos = absoluteIn(
      rightBgSpritePos, config.levelBgWidth, config.levelBgHeight,
      17 + 20 * i, config.levelSelectCardWidth,
      17, config.levelSelectCardHeight,
    );
    const cardPos = absoluteIn(
      rightBgSpritePos, config.levelBgWidth, config.levelBgHeight,
      15 + 20 * i, config.levelSelectCardWidth,
      15, config.levelSelectCardHeight,
    );

    const cardSlot = createPoolCardSlot(gameRefs.levelSelectData.cardSlotPool, cardSlotPos);
    const cardId = gameRefs.saveFile.levelSolutions[levelId][solId].cardIds[i];
    if (cardId !== undefined) {
      const card = createPoolLevelSelectCard(game, gameRefs, gameRefs.levelSelectData.cardPool, gameRefs.levelSelectData.cardSlotPool, cardPos, cardId, cardId, cardSlot, "friendly", levelId, solId);
      cardSlot.data.card = card;
      // remove this card once from the supply pool
      const index = supplyPool.indexOf(cardId);
      if (index !== -1) {
        supplyPool[index] = undefined;
      }
    }
    cardSlot.data.type = "deploy";
    cardSlot.data.index = i;
  }
  // level card slots and cards
  for (let i = 0; i < supplySize; i++) {
    const cardSlotPos = absoluteIn(
      leftBgSpritePos, config.levelBgWidth, config.levelBgHeight,
      17 + 20 * i, config.levelSelectCardWidth,
      17, config.levelSelectCardHeight,
    );
    const cardPos = absoluteIn(
      leftBgSpritePos, config.levelBgWidth, config.levelBgHeight,
      15 + 20 * i, config.levelSelectCardWidth,
      15, config.levelSelectCardHeight,
    );
    const cardSlot = createPoolCardSlot(gameRefs.levelSelectData.cardSlotPool, cardSlotPos);
    const cardId = supplyPool[i];
    if (cardId !== undefined) {
      const card = createPoolLevelSelectCard(game, gameRefs, gameRefs.levelSelectData.cardPool, gameRefs.levelSelectData.cardSlotPool, cardPos, cardId, cardId, cardSlot, "friendly", levelId, solId);
      cardSlot.data.card = card;
    }
    cardSlot.data.type = "supply";
    cardSlot.data.index = i;
  }
  //levelSelect.slots = slots;
}

type LevelSelectCard = GSprite<ButtonValues & {
  cardId: string,
  type: TargetType,
  levelId: string,
  solId: number,
  hoverSlot: Phaser.Sprite | undefined,
  resetSlot: Phaser.Sprite,
}>;

function createPoolLevelSelectCard(
  game: Phaser.Game,
  gameRefs: GameRefs,
  pool: Phaser.Group,
  slotPool: Phaser.Group,
  pos: Position,
  key: string,
  cardId: string,
  resetSlot: Phaser.Sprite,
  type: TargetType,
  levelId: string,
  solId: number,
): LevelSelectCard {
  const card = createButtonInPool(
    game,
    pool,
    pos,
    { cardId, type, hoverSlot: <Phaser.Sprite | undefined>undefined, resetSlot, levelId, solId },
    key,
    {
      hoverOver: () => {
        const x = card.x + config.levelSelectCardWidth + 10;
        const y = card.y;
        applyScreenEvent(new SE.ShowHoverCard(card.data.type, card.data.cardId, x, y), game, gameRefs);
      },
      hoverOut: () => {
        applyScreenEvent(new SE.ClearHoverCard(), game, gameRefs);
      },
      dragStart: () => {
        applyScreenEvent(new SE.ClearHoverCard(), game, gameRefs);
      },
      dragUpdate: () => {
        console.log("UPDATE");
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
      },
      dragStop: () => {
        if (card.data.hoverSlot === undefined) {
          card.data.resetSlot.data.card = card;
          moveToSlot(card, card.data.resetSlot);
        } else {
          moveToSlot(card, card.data.hoverSlot);
          let from = { pos: card.data.resetSlot.data.index, type: card.data.resetSlot.data.type };
          let to = { pos: card.data.hoverSlot.data.index, type: card.data.hoverSlot.data.type };
          applyScreenEvent(
            new SE.DeployCard(card.data.cardId, card.data.solId, from, to),
            game, gameRefs
          );
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
      },
    },
  );
  return card;
}

function moveToSlot(
  card: Phaser.Sprite,
  slot: Phaser.Sprite,
) {
  card.x = slot.x - (2 * config.levelBgWidth / 100);
  card.y = slot.y - (2 * config.levelBgHeight / 100);
}