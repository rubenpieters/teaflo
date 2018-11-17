import { GameRefs, LevelSelect, levelSelectToGameScreen } from "../states/game";
import { createPosition, absoluteIn } from "../util/position";
import { config } from "../config";
import { levelSelect_LevelSlots } from "./general";
import { createButton } from "../util/button";
import { levelDataMap } from "../gameData";
import { createPoolCardSlot } from "../util/poolCardSlot";
import { createPoolLevelSelectCard } from "../util/poolLevelSelectCard";

export function drawLevelInfo(
  game: Phaser.Game,
  gameRefs: GameRefs,
  hoverViewPool: Phaser.Group,
  levelId: string,
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
      /*const cards = levelSelect.slots.map(x => {
        if (x.data.card !== undefined) {
          return x.data.card.data.cardId
        } else {
          return undefined;
        }
      });
      levelSelectToGameScreen(game, cards, levelId);*/
    }
  );
  gameRefs.levelSelectData.startBtn = startBtn;

  // level card slots and cards
  let i = 0;
  for (const cardId of levelDataMap[levelId].cardIds) {
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
    const card = createPoolLevelSelectCard(gameRefs.levelSelectData.cardPool, gameRefs.levelSelectData.cardSlotPool, hoverViewPool, cardPos, cardId, cardId);
    cardSlot.data.card = card;
    card.data.resetSlot = cardSlot;

    i += 1;
  }

  // solution card slots
  const slots: Phaser.Sprite[] = [];
  for (let i = 0; i < levelDataMap[levelId].slots; i++) {
    const cardSlotPos = absoluteIn(
      rightBgSpritePos, config.levelBgWidth, config.levelBgHeight,
      17 + 20 * i, config.levelSelectCardWidth,
      17, config.levelSelectCardHeight,
    );

    const cardSlot = createPoolCardSlot(gameRefs.levelSelectData.cardSlotPool, cardSlotPos);
    slots.push(cardSlot);
  }
  //levelSelect.slots = slots;
}