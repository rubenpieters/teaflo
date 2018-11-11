import { actSelect_Main } from "src/app/screens/actSelect";
import { gameScreen_Main } from "../screens/gameScreen";
import { SaveFileV1 } from "../savefile/rep";

export type GameRefs = {
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
  rightBg?: Phaser.Sprite,
  leftBg?: Phaser.Sprite,
  startBtn?: Phaser.Sprite,
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
  },
  levelUnlocked: {
    "a1_l1": "unlocked",
  },
}

export default class Game extends Phaser.State {
  public init(): void {
    // NOTE: order of creating these groups determines their z-index
    const actSelectBtnPool = new Phaser.Group(this.game);
    const levelSelectBtnPool = new Phaser.Group(this.game);
    levelSelect = {
      group: new Phaser.Group(this.game),
      cardSlotPool: new Phaser.Group(this.game),
      cardPool: new Phaser.Group(this.game),
      slots: [],
    };
    gameScreen = {
      group: new Phaser.Group(this.game),
      unitPool: new Phaser.Group(this.game),
    }
    gameRefs = {
      actSelectBtnPool: actSelectBtnPool,
      levelSelectBtnPool: levelSelectBtnPool,
      hoverViewPool: new Phaser.Group(this.game),
      // TODO: read file from somewhere
      saveFile: newSaveFile,
    }
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, gameRefs, levelSelect);
  }
}

export function levelSelectToGameScreen(
  game: Phaser.Game,
  cards: string[]
) {
  setSelectScreenVisible(false);
  setGameScreenVisible(true);
  const gameState = {
    cardIds: cards,
  }
  gameScreen_Main(game, gameState, gameScreen);
}

export function gameScreenToLevelSelect(
  _game: Phaser.Game,
) {
  setGameScreenVisible(false);
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
}

function setGameScreenVisible(
  visible: boolean,
) {
  gameScreen.group.visible = visible;
}