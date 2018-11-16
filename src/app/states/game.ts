import { actSelect_Main } from "src/app/screens/general";
import { gameScreen_Main } from "../screens/gameScreen";
import { SaveFileV1 } from "../savefile/rep";
import { applyUnlocks } from "../savefile/unlocks";
import { ActSelectData } from "../screens/actSelect";
import { applyScreenEvent, mkChangeAct } from "../util/screenEvents";

export type GameRefs = {
  actSelectData: ActSelectData,
  actSelectBtnPool: Phaser.Group,
  levelSelectBtnPool: Phaser.Group,
  hoverViewPool: Phaser.Group,
  saveFile: SaveFileV1,
}
let gameRefs: GameRefs;

export type LevelSelect = {
  group: Phaser.Group,
  cardPool: Phaser.Group,
  cardSlotPool: Phaser.Group,
  solBtnPool: Phaser.Group,
  rightBg?: Phaser.Sprite,
  leftBg?: Phaser.Sprite,
  startBtn?: Phaser.Sprite,
  addSolBtn?: Phaser.Sprite,
  slots: Phaser.Sprite[],
};
let levelSelect: LevelSelect;

export type GameScreen = {
  group: Phaser.Group,
  unitPool: Phaser.Group,
  exitBtn?: Phaser.Sprite,
  victoryBtn?: Phaser.Sprite,
}
let gameScreen: GameScreen;

const newSaveFile: SaveFileV1 = {
  version: "V1",
  actUnlocked: {
    0: "unlocked",
    1: "unlocked",
  },
  levelUnlocked: {
    "a1_l1": "unlocked",
    "a1_l2": "unlocked",
    "a2_l1": "unlocked",
    "a2_l2": "unlocked",
  },
  levelSolutions: {
    "a1_l1": [{
      solution: { win: false, },
      cardIds: [],
    }],
  },
  activeSolutions: {
    "a1_l1": 0,
  },
  activeAct: 0,
  activeLevel: "a1_l1",
}

export default class Game extends Phaser.State {
  public init(): void {
    // NOTE: order of creating these groups determines their z-index
    const actSelectBtnPool = new Phaser.Group(this.game);
    const actSelectData = {
      btnPool: new Phaser.Group(this.game),
    }
    const levelSelectBtnPool = new Phaser.Group(this.game);
    levelSelect = {
      group: new Phaser.Group(this.game),
      cardSlotPool: new Phaser.Group(this.game),
      cardPool: new Phaser.Group(this.game),
      solBtnPool: new Phaser.Group(this.game),
      slots: [],
    };
    gameScreen = {
      group: new Phaser.Group(this.game),
      unitPool: new Phaser.Group(this.game),
    }
    gameRefs = {
      actSelectData,
      actSelectBtnPool,
      levelSelectBtnPool,
      hoverViewPool: new Phaser.Group(this.game),
      // TODO: read file from somewhere
      saveFile: newSaveFile,
    }
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    applyScreenEvent(mkChangeAct(0), this.game, gameRefs);
  }
}

export function levelSelectToGameScreen(
  game: Phaser.Game,
  cards: string[],
  levelId: string,
) {
  setSelectScreenVisible(false);
  setGameScreenVisible(true);
  const gameState = {
    cardIds: cards,
  }
  gameScreen_Main(game, gameState, gameScreen, levelId);
}

export function gameScreenToLevelSelect(
  game: Phaser.Game,
  unlock: string | undefined,
) {
  setGameScreenVisible(false);
  if (unlock !== undefined) {
    applyUnlocks(gameRefs.saveFile, unlock);
    actSelect_Main(game, gameRefs, levelSelect);
  }
  setSelectScreenVisible(true);
}

function setSelectScreenVisible(
  visible: boolean,
) {
  gameRefs.actSelectBtnPool.visible = visible;
  gameRefs.levelSelectBtnPool.visible = visible;
  levelSelect.group.visible = visible;
  levelSelect.cardPool.visible = visible;
  levelSelect.cardSlotPool.visible = visible;
  levelSelect.solBtnPool.visible = visible;
}

function setGameScreenVisible(
  visible: boolean,
) {
  gameScreen.group.visible = visible;
}