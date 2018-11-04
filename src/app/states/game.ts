import { actSelect_Main } from "../screens/actSelect";

let actSelectPool: Phaser.Group;
let levelSelectPool: Phaser.Group;

export default class Game extends Phaser.State {
  public init(): void {
    actSelectPool = new Phaser.Group(this.game);
    levelSelectPool = new Phaser.Group(this.game);
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, actSelectPool, levelSelectPool);
  }

}