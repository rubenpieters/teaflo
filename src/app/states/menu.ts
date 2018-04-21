import { changeSelectedScreen, addSelectedScreenCallback } from "src/app/appstate";

export default class Menu extends Phaser.State {
  public create(): void {
    this.stage.backgroundColor = 0x5d5d5d;

    const top1: Phaser.Text = this.add.text(0, 0, "Home", {
      font: "60px Indie Flower",
      fill: "#F08080",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top1.setTextBounds(0, 0, 250, 100);
    top1.inputEnabled = true;
    top1.events.onInputDown.add(() => changeSelectedScreen("Home"));

    const top2: Phaser.Text = this.add.text(0, 0, "Play", {
      font: "60px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top2.setTextBounds(250, 0, 250, 100);
    top2.inputEnabled = true;
    top2.events.onInputDown.add(() => changeSelectedScreen("Play"));

    // Menu Screen

    const menuGroup = this.game.add.group();

    const home1: Phaser.Text = this.add.text(0, 0, "Top Solutions", {
      font: "50px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    }, menuGroup);
    home1.setTextBounds(400, 300, 150, 75);

    // Play Screen

    const playGroup = this.game.add.group();

    const playResourceMenu = this.game.add.graphics(0, 600, playGroup);
    playResourceMenu.beginFill(0x227744);
    playResourceMenu.drawRect(0, 0, 800, 200);
    playResourceMenu.endFill();

    playGroup.visible = false;

    addSelectedScreenCallback(screen => { switch (screen) {
      case "Home": {
        top1.fill = "#F08080";
        top2.fill = "#77BFA3";
        menuGroup.visible = true;
        playGroup.visible = false;
        break;
      }
      case "Play": {
        top1.fill = "#77BFA3";
        top2.fill = "#F08080";
        menuGroup.visible = false;
        playGroup.visible = true;
        break;
      }
    }});
  }
}
