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
    this.game.load.spritesheet("btn_log", "textures/btn_log.png", 960, 100);
    this.game.load.image("bg_level", "textures/bg_level.png");
    this.game.load.image("bg_hover", "textures/bg_hover.png");
    this.game.load.image("bg_hover_2", "textures/bg_hover_2.png");
    this.game.load.image("card1", "textures/card1.png");
    this.game.load.image("card2", "textures/card2.png");
    this.game.load.image("card3", "textures/card3.png");
    this.game.load.image("card4", "textures/card4.png");
    this.game.load.image("card5", "textures/card5.png");
    this.game.load.image("card6", "textures/card6.png");
    this.game.load.image("card7", "textures/card7.png");
    this.game.load.image("fr_unit_a1_l1_01", "textures/fr_unit_a1_l1_01.png");
    this.game.load.image("fr_unit_a1_l2_01", "textures/fr_unit_a1_l2_01.png");
    this.game.load.image("fr_unit_a1_l2_02", "textures/fr_unit_a1_l2_02.png");
    this.game.load.image("fr_unit_a1_l2_03", "textures/fr_unit_a1_l2_03.png");
    this.game.load.image("en_unit_a1_l1_01", "textures/en_unit_a1_l1_01.png");
    this.game.load.image("fr_unit_a1_l1_01_ab1", "textures/fr_unit_a1_l1_01_ab1.png");
    this.game.load.image("fr_unit_a1_l1_01_ab2", "textures/fr_unit_a1_l1_01_ab2.png");
    this.game.load.image("fr_unit_a1_l2_01_ab1", "textures/fr_unit_a1_l2_01_ab1.png");
    this.game.load.image("hp", "textures/hp.png");
    this.game.load.image("ch", "textures/ch.png");
    this.game.load.image("th", "textures/th.png");
    this.game.load.image("route", "textures/slot.jpg");
    this.game.load.image("current_route", "textures/current_route.png");
    this.game.load.image("tr_weak", "textures/trigger_weak.png");
    this.game.load.image("tr_strong", "textures/trigger_strong.png");
    this.game.load.image("tr_armor", "textures/trigger_armor.png");
    this.game.load.image("ability", "textures/ability.png");
    this.game.load.spritesheet("card_slot", "textures/card_slot.png", 150, 300);
    this.game.load.spritesheet("arrow", "textures/arrow.png", 150, 100);

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
