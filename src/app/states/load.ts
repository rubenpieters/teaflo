import { config } from "src/app/config";

export default class Load extends Phaser.State {
  public init(): void {
    this.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.scale.maxWidth = config.gameWidth;
    this.scale.maxHeight = config.gameHeight;
    this.scale.pageAlignHorizontally = true;
    this.scale.pageAlignVertically = true;
  }

  public create(): void {
    const loadingText: Phaser.Text = this.game.add.text(0, 0, "loading...", {
      fill: "#D3D3D3",
      boundsAlignH: "center",
      boundsAlignV: "middle"
    });

    loadingText.setTextBounds(200, 200, 200, 100);

    this.startMenu();
  }

  public preload(): void {
    this.game.load.image("card1", "textures/card1.jpg");
    this.game.load.image("slot", "textures/slot.jpg");
  }

  private startMenu(): void {
    this.game.state.start("menu");
  }
}
