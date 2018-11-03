import { actSelect_Main } from "../screens/actSelect";

let actSelected: number = 0;
let actSelectGroup: Phaser.Group;

export default class Game extends Phaser.State {
  public init(): void {
    actSelectGroup = new Phaser.Group(this.game);
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    actSelect_Main(this.game, actSelectGroup);
  }

}