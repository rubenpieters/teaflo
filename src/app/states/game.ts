import { actSelect_Main } from "src/app/screens/actSelect";
import { gameScreen_Main } from "../screens/gameScreen";

let actSelectBtnPool: Phaser.Group;
let levelSelectBtnPool: Phaser.Group;

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
}
let gameScreen: GameScreen;

export default class Game extends Phaser.State {
  public init(): void {
    actSelectBtnPool = new Phaser.Group(this.game);
    levelSelectBtnPool = new Phaser.Group(this.game);
    // NOTE: order of creating these groups determines their z-index
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
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, actSelectBtnPool, levelSelectBtnPool, levelSelect);
  }
}

export function levelSelectToGameScreen(
  game: Phaser.Game,
  cards: string[]
) {
  setSelectScreenVisible(false);
  setGameScreenVisible(true);
  gameScreen_Main(game, cards, gameScreen);
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
  actSelectBtnPool.visible = visible;
  levelSelectBtnPool.visible = visible;
  levelSelect.group.visible = visible;
  levelSelect.cardPool.visible = visible;
  levelSelect.cardSlotPool.visible = visible;
}

function setGameScreenVisible(
  visible: boolean,
) {
  gameScreen.group.visible = visible;
}