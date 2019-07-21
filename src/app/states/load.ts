import { Settings } from "../data/settings";

let loadingText: Phaser.Text;
let ready: boolean = false;

export default class Load extends Phaser.State {
  settings: Settings = undefined as any;

  public init(settings: Settings): void {
    this.settings = settings;
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
    this.game.load.image("bg0", "assets/textures/paper_bg2.jpg");
    this.game.load.image("bg1", "assets/textures/paper_bg3.jpg");
    this.game.load.image("bg2", "assets/textures/paper_bg4.jpg");
    this.game.load.image("bg3", "assets/textures/paper_bg5.jpg");
    this.game.load.image("bg4", "assets/textures/paper_bg1.jpg");
    this.game.load.atlasJSONHash('atlas1', 'assets/atlas/atlas1.png', 'assets/atlas/atlas1.json');

    // load shaders
    this.game.load.shader("blue-glow", "assets/shaders/gaussian-blue-pulse.frag");
    this.game.load.shader("red-glow", "assets/shaders/gaussian-red-pulse.frag");
    this.game.load.shader("blue-flame", "assets/shaders/blue-flame.frag");

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
    this.game.state.start("game", undefined, undefined, this.settings);
  }
}
