import { actSelect_Main } from "../screens/actSelect";

let actSelectBtnPool: Phaser.Group;
let levelSelectBtnPool: Phaser.Group;

export default class Game extends Phaser.State {
  public init(): void {
    actSelectBtnPool = new Phaser.Group(this.game);
    levelSelectBtnPool = new Phaser.Group(this.game);
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, actSelectBtnPool, levelSelectBtnPool);
  }

}