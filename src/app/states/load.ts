import { settings } from "../data/settings";

let loadingText: Phaser.Text;
let ready: boolean = false;

export default class Load extends Phaser.State {
  public init(): void {
    this.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.scale.maxWidth = settings.gameWidth;
    this.scale.maxHeight = settings.gameHeight;
    this.scale.pageAlignHorizontally = true;
    this.scale.pageAlignVertically = true;

    this.game.canvas.oncontextmenu = e => e.preventDefault();

    loadingText = this.game.add.text(0, 0, "loading...", {
      fill: "#D3D3D3",
      fontSize: 100,
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });

    loadingText.setTextBounds(
      0,
      settings.gameHeight / 3,
      settings.gameWidth,
      settings.gameHeight
    );
  }

  public create(): void {
  }

  public preload(): void {
    // load sprites
    this.game.load.spritesheet("btn", "textures/bmark.png", 200, 400);

    // indicate loading is done
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
