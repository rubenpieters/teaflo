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
  levelSelect: LevelSelect,
  hoverViewPool: Phaser.Group,
  levelId: string,
) {
  levelSelect.cardSlotPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  levelSelect.cardPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  // left background
  const leftBgSpritePos = createPosition(
    "right", 1650, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.leftBg === undefined) {
    const leftBgSprite = game.add.sprite(leftBgSpritePos.xMin, leftBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.leftBg = leftBgSprite;
  }

  // save slots
  levelSelect_LevelSlots(game, gameRefs, levelSelect, levelId);

  // right background
  const rightBgSpritePos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.rightBg === undefined) {
    const rightBgSprite = game.add.sprite(rightBgSpritePos.xMin, rightBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.rightBg = rightBgSprite;
  }

  // start game button
  if (levelSelect.startBtn !== undefined) {
    levelSelect.startBtn.destroy();
  }
  const startBtnPos = absoluteIn(
    rightBgSpritePos, config.levelBgWidth, config.levelBgHeight,
    70, config.levelButtonWidth,
    90, config.levelButtonHeight,
  );
  const startBtn = createButton(game, levelSelect.group, startBtnPos, "Start", "btn_level",
    () => {
      const cards = levelSelect.slots.map(x => {
        if (x.data.card !== undefined) {
          return x.data.card.data.cardId
        } else {
          return undefined;
        }
      });
      levelSelectToGameScreen(game, cards, levelId);
    }
  );
  levelSelect.startBtn = startBtn;

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
    const cardSlot = createPoolCardSlot(levelSelect.cardSlotPool, cardSlotPos);
    const card = createPoolLevelSelectCard(levelSelect.cardPool, levelSelect.cardSlotPool, hoverViewPool, cardPos, cardId, cardId);
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

    const cardSlot = createPoolCardSlot(levelSelect.cardSlotPool, cardSlotPos);
    slots.push(cardSlot);
  }
  levelSelect.slots = slots;
}