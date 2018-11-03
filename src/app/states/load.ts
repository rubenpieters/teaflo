import { config } from "src/app/config";

let loadingText: Phaser.Text;
let ready: boolean = false;

export default class Load extends Phaser.State {
  public init(): void {
    this.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.scale.maxWidth = config.gameWidth;
    this.scale.maxHeight = config.gameHeight;
    this.scale.pageAlignHorizontally = true;
    this.scale.pageAlignVertically = true;

    this.game.canvas.oncontextmenu = e => e.preventDefault();

    loadingText = this.game.add.text(0, 0, "loading...", {
      fill: "#D3D3D3",
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle"
    });

    loadingText.setTextBounds(
      0,
      config.gameHeight / 3,
      config.gameWidth,
      config.gameHeight
    );
  }

  public create(): void {
  }

  public preload(): void {
    this.game.load.image("card1", "textures/card1.jpg");
    this.game.load.image("slot", "textures/slot.jpg");
    this.game.load.image("ally", "textures/ally.jpg");
    this.game.load.image("item", "textures/item.jpg");
    this.game.load.image("rest", "textures/rest.jpg");

    loadingText.setText("Click anywhere to continue...");
    ready = true;
  }

  public update(): void {
    if (ready && this.game.input.activePointer.leftButton.isDown) {
      this.startGame();
    }
  }

  private startGame(): void {
    this.game.state.start("game");
  }
}
