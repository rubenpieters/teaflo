import { config } from "src/app/config";
import { createPoolButton } from "src/app/util/poolButton";
import { createButton } from "src/app/util/button";
import { createPosition, absoluteIn } from "src/app/util/position";
import { LevelSelect, levelSelectToGameScreen, GameRefs } from "src/app/states/game";
import { createPoolLevelSelectCard } from "../util/poolLevelSelectCard";
import { createPoolCardSlot } from "../util/poolCardSlot";
import { actAvailable, levelAvailable } from "../savefile/rep";
import { createLockedPoolButton } from "../util/lockedPoolButton";

// act -> button string mapping
export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
};

export function actSelect_Main(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelSelect: LevelSelect,
) {
  gameRefs.actSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );
    
    let button: Phaser.Sprite;
    if (actAvailable(gameRefs.saveFile, Number(actNumber))) {
      button = createPoolButton(game, gameRefs.actSelectBtnPool, pos, btnString, "btn_act",
        () => levelSelect_Main(game, gameRefs, levelSelect, Number(actNumber))
      );
    } else {
      button = createLockedPoolButton(game, gameRefs.actSelectBtnPool, pos, btnString, "btn_act");
    }
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
export const levelMap: { [key: number]: string[] } = {
  0: ["a1_l1", "a1_l2"],
  1: ["a2_l1", "a2_l2", "a2_l3"],
}

export function levelSelect_Main(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelSelect: LevelSelect,
  act: number,
) {
  gameRefs.levelSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const levelId of levelMap[act]) {
    let btnString = levelId;
  
    const pos = createPosition(
      "left", 250, config.levelButtonWidth,
      "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
    );

    let button: Phaser.Sprite;
    if (levelAvailable(gameRefs.saveFile, levelId)) {
      button = createPoolButton(game, gameRefs.levelSelectBtnPool, pos, btnString, "btn_level",
        () => levelSelect_Info(game, gameRefs, levelSelect, gameRefs.hoverViewPool, levelId)
      );
    } else {
      button = createLockedPoolButton(game, gameRefs.levelSelectBtnPool, pos, btnString, "btn_level");
    }
    if (i === 0) {
      first = button;
    }

    i += 1;
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
  "a1_l2": { cardIds: [], slots: 0 },
  "a2_l1": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l2": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l3": { cardIds: ["card1", "card2", "card3"], slots: 4 },
}

export function levelSelect_Info(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelSelect: LevelSelect,
  hoverViewPool: Phaser.Group,
  levelId: string,
) {
  levelSelect.cardSlotPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  levelSelect.cardPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  const leftBgSpritePos = createPosition(
    "right", 1650, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.leftBg === undefined) {
    const leftBgSprite = game.add.sprite(leftBgSpritePos.xMin, leftBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.leftBg = leftBgSprite;
  }

  levelSelect_LevelSlots(game, gameRefs, levelSelect, levelId);

  const rightBgSpritePos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  if (levelSelect.rightBg === undefined) {
    const rightBgSprite = game.add.sprite(rightBgSpritePos.xMin, rightBgSpritePos.yMin, "bg_level", undefined, levelSelect.group);
    levelSelect.rightBg = rightBgSprite;
  }

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

export function levelSelect_LevelSlots(
  game: Phaser.Game,
  gameRefs: GameRefs,
  levelSelect: LevelSelect,
  levelId: string,
) {
  levelSelect.solBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());

  let i = 0;
  // the savefile has no solutions yet, then create one
  if (gameRefs.saveFile.levelSolutions[levelId] === undefined) {
    gameRefs.saveFile.levelSolutions[levelId] = [{
      solution: { win: false, },
      cardIds: [],
    }];
  }
  // create a button for each solution in the savefile
  let active: Phaser.Sprite | undefined = undefined;
  for (const sol of gameRefs.saveFile.levelSolutions[levelId]) {
    const solBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    const solBtn = createPoolButton(game, levelSelect.solBtnPool, solBtnPos, `sol ${i}`, "btn_level",
      () => console.log("choose sol")
    );
    if (i === gameRefs.saveFile.activeSolutions[levelId]) {
      active = solBtn;
    }
    i += 1;
  }
  if (active !== undefined) {
    active.events.onInputUp.dispatch({ force: true });
  }
  // create the '+' button to add a solution
  if (levelSelect.addSolBtn !== undefined) {
    levelSelect.addSolBtn.destroy();
    levelSelect.addSolBtn = undefined;
  }
  if (gameRefs.saveFile.levelSolutions[levelId].length < 5) {
    const addSolBtnPos = createPosition(
      "right", 1250, config.levelButtonWidth,
      "top", 500 + 250 * i, config.levelButtonHeight,
    );
    const addSolBtn = createButton(game, levelSelect.group, addSolBtnPos, "+", "btn_level",
      () => {
        gameRefs.saveFile.levelSolutions[levelId].push({
          solution: { win: false, },
          cardIds: [],
        });
        levelSelect_LevelSlots(game, gameRefs, levelSelect, levelId);
      }
    );
    levelSelect.addSolBtn = addSolBtn;
  }
}