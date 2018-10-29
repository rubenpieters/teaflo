import { config } from "src/app/config";

export default class Game extends Phaser.State {
  public init(): void {
    this.game.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.game.scale.maxWidth = config.gameWidth;
    this.game.scale.maxHeight = config.gameHeight;
    this.game.scale.pageAlignHorizontally = true;
    this.game.scale.pageAlignVertically = true;

    this.game.canvas.oncontextmenu = e => e.preventDefault();
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

  }
}