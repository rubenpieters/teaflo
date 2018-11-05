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
      boundsAlignV: "middle",
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
    this.game.load.spritesheet("btn_act", "textures/btn_act.png", 200, 200);
    this.game.load.spritesheet("btn_level", "textures/btn_level.png", 400, 200);
    this.game.load.image("bg_level", "textures/bg_level.png");
    this.game.load.image("card1", "textures/card1.png");
    this.game.load.image("card2", "textures/card2.png");
    this.game.load.image("card3", "textures/card3.png");

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
