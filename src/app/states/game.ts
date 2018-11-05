import { actSelect_Main } from "src/app/screens/actSelect";

let actSelectBtnPool: Phaser.Group;
let levelSelectBtnPool: Phaser.Group;

export type LevelSelect = {
  group: Phaser.Group,
  rightBg?: Phaser.Sprite,
  leftBg?: Phaser.Sprite,
  startBtn?: Phaser.Sprite,
};
let levelSelect: LevelSelect;

export default class Game extends Phaser.State {
  public init(): void {
    actSelectBtnPool = new Phaser.Group(this.game);
    levelSelectBtnPool = new Phaser.Group(this.game);
    levelSelect = { group: new Phaser.Group(this.game) };
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, actSelectBtnPool, levelSelectBtnPool, levelSelect);
  }

}