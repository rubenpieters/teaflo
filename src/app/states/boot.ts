import { Settings } from "../data/settings";


export default class Boot extends Phaser.State {
  settings: Settings = undefined as any;

  public init(settings: Settings): void {
    this.settings = settings;
    this.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.scale.maxWidth = settings.gameWidth;
    this.scale.maxHeight = settings.gameHeight;
    this.scale.pageAlignHorizontally = true;
    this.scale.pageAlignVertically = true;
  }

  public create(): void {
    this.game.state.start("load", undefined, undefined, this.settings);
  }
}
