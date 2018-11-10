import { config } from "src/app/config";
import { createPoolButton } from "src/app/util/poolButton";
import { createButton } from "src/app/util/button";
import { createPosition, absoluteIn } from "src/app/util/position";
import { LevelSelect, levelSelectToGameScreen } from "src/app/states/game";
import { createPoolLevelSelectCard } from "../util/poolLevelSelectCard";
import { createPoolCardSlot } from "../util/poolCardSlot";

// act -> button string mapping
export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
  2: "...",
};

export function actSelect_Main(
  game: Phaser.Game,
  actSelectBtnPool: Phaser.Group,
  levelSelectBtnPool: Phaser.Group,
  levelSelect: LevelSelect,
) {
  actSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );

    const button = createPoolButton(game, actSelectBtnPool, pos, btnString, "btn_act",
      () => levelSelect_Main(game, levelSelectBtnPool, levelSelect, Number(actNumber))
    );
    if (i === 0) {
      first = button;
    }

    i += 1;
  }

  if (first !== undefined) {
    first.events.onInputUp.dispatch({ force: true });
  }
}

// act -> level id mapping
export const levelMap: { [key: number]: string[] | "info" } = {
  0: ["a1_l1"],
  1: ["a2_l1", "a2_l2", "a2_l3"],
  2: "info",
}

export function levelSelect_Main(
  game: Phaser.Game,
  levelSelectBtnPool: Phaser.Group,
  levelSelect: LevelSelect,
  act: number,
) {
  levelSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const levelId of levelMap[act]) {
    if (levelMap[act] === "info") {

    } else {
      let btnString = levelId;
  
      const pos = createPosition(
        "left", 250, config.levelButtonWidth,
        "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
      );
  
      const button = createPoolButton(game, levelSelectBtnPool, pos, btnString, "btn_level",
        () => levelSelect_Info(game, levelSelect, levelId)
      );
      if (i === 0) {
        first = button;
      }
  
      i += 1;
    }
  }

  if (first !== undefined) {
    first.events.onInputUp.dispatch({ force: true });
  }
}

type LevelData = {
  cardIds: string[],
  slots: number,
}

// level id -> card id mapping
export const levelDataMap: { [key: string]: LevelData } = {
  "a1_l1": { cardIds: [], slots: 0 },
  "a2_l1": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l2": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l3": { cardIds: ["card1", "card2", "card3"], slots: 4 },
}

export function levelSelect_Info(
  game: Phaser.Game,
  levelSelect: LevelSelect,
  levelId: string,
) {
  const leftBgSpritePos = createPosition(
    "right", 1500, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.leftBg === undefined) {
    const leftBgSprite = game.add.sprite(leftBgSpritePos.xMin, leftBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.leftBg = leftBgSprite;
  }

  const rightBgSpritePos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.rightBg === undefined) {
    const rightBgSprite = game.add.sprite(rightBgSpritePos.xMin, rightBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.rightBg = rightBgSprite;
  }

  if (levelSelect.startBtn === undefined) {
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
        levelSelectToGameScreen(game, cards);
      }
    );
    levelSelect.startBtn = startBtn;
  }

  levelSelect.cardPool.forEachAlive((x: Phaser.Sprite) => x.kill());
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
    const card = createPoolLevelSelectCard(levelSelect.cardPool, levelSelect.cardSlotPool, cardPos, cardId, cardId);
    cardSlot.data.card = card;
    card.data.resetSlot = cardSlot;

    i += 1;
  }

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
