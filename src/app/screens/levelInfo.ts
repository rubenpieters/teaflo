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
import { activeSolInfo } from "../savefile/rep";

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

  // deploy slots
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
    const cardId = gameRefs.saveFile.levelSolutions[levelId][solId].deploy[i];
    if (cardId !== undefined) {
      const card = createPoolLevelSelectCard(
        game, gameRefs, gameRefs.levelSelectData.cardPool, gameRefs.levelSelectData.cardSlotPool,
        cardPos, cardId, cardId, cardSlot, levelId, solId
      );
      cardSlot.data.card = card;
    }
    cardSlot.data.index = i;
    cardSlot.data.type = "deploy";
  }
  
  // supply slots
  const supplySize = levelDataMap[levelId].cardIds.length;
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
    const cardId = gameRefs.saveFile.levelSolutions[levelId][solId].supply[i];
    if (cardId !== undefined) {
      const card = createPoolLevelSelectCard(
        game, gameRefs, gameRefs.levelSelectData.cardPool, gameRefs.levelSelectData.cardSlotPool,
        cardPos, cardId, cardId, cardSlot, levelId, solId
      );
      cardSlot.data.card = card;
    }
    cardSlot.data.index = i;
    cardSlot.data.type = "supply";
  }
}

type LevelSelectCard = GSprite<ButtonValues & {
  cardId: string,
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
  levelId: string,
  solId: number,
): LevelSelectCard {
  const card = createButtonInPool(
    game,
    pool,
    pos,
    { cardId, hoverSlot: <Phaser.Sprite | undefined>undefined, resetSlot, levelId, solId },
    spriteMap[key],
    {
      clickRight: () => {
        const firstEmptyDeploy = activeSolInfo(gameRefs.saveFile).deploy.findIndex(x => x === undefined);
        if (firstEmptyDeploy !== -1) {
          const from = { pos: card.data.resetSlot.data.index, type: card.data.resetSlot.data.type };
          const to = { pos: firstEmptyDeploy, type: <"deploy">"deploy" };
          applyScreenEvent(
            new SE.DeployCard(from, to),
            game, gameRefs
          );
        }
      },
      hoverOver: () => {
        // const x = card.x + config.levelSelectCardWidth + 10;
        // const y = card.y;
        // applyScreenEvent(new SE.ShowHoverCard(card.data.type, card.data.cardId, x, y), game, gameRefs);
      },
      hoverOut: () => {
        // applyScreenEvent(new SE.ClearHoverCard(), game, gameRefs);
      },
      dragStart: () => {
        // applyScreenEvent(new SE.ClearHoverCard(), game, gameRefs);
      },
      dragUpdate: () => {
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
          // hover slot is undefined: move it back to its reset slot
          applyScreenEvent(
            new SE.ResetCard(),
            game, gameRefs
          );
        } else {
          // hover slot is defined: move to this new slot
          let from = { pos: card.data.resetSlot.data.index, type: card.data.resetSlot.data.type };
          let to = { pos: card.data.hoverSlot.data.index, type: card.data.hoverSlot.data.type };
          applyScreenEvent(
            new SE.DeployCard(from, to),
            game, gameRefs
          );
        }
      },
    },
  );
  return card;
}