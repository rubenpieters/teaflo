import { changeSelectedScreen, addSelectedScreenCallback, addConnectedCallback } from "src/app/appstate";
import { connectToServer } from "src/app/network/network";

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

    const connectionIndicator: Phaser.Text = this.add.text(620, 10, "No Connection", {
      font: "15px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });

    // callbacks

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

    addConnectedCallback(connected => {
      if (connected) {
        connectionIndicator.setText("Connected");
      } else {
        connectionIndicator.setText("No Connection");
      }
    });

    // Menu Screen

    const menuGroup = this.game.add.group();

    const home1: Phaser.Text = this.add.text(0, 0, "Current Board", {
      font: "35px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    home1.setTextBounds(50, 100, 150, 75);

    const home2: Phaser.Text = this.add.text(0, 0, "Top Solutions", {
      font: "35px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "left",
      boundsAlignV: "middle",
    }, menuGroup);
    home2.setTextBounds(50, 200, 150, 75);

    // Play Screen

    const playGroup = this.game.add.group();

    const playResourceMenu = this.game.add.graphics(0, 600, playGroup);
    playResourceMenu.beginFill(0x227744);
    playResourceMenu.drawRect(0, 0, 800, 200);
    playResourceMenu.endFill();

    playGroup.visible = false;

    connectToServer();
  }
}
