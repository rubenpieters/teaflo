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
    this.game.load.image("bg0", "textures/paper_bg2.jpg");
    this.game.load.image("bg1", "textures/paper_bg3.jpg");
    this.game.load.image("bg2", "textures/paper_bg4.jpg");
    this.game.load.image("bg3", "textures/paper_bg5.jpg");
    this.game.load.image("bg4", "textures/paper_bg1.jpg");
    this.game.load.atlasJSONHash('atlas1', 'assets/atlas1.png', 'assets/atlas1.json');

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
