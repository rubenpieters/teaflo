import { changeSelectedScreen, getSelectedScreen, addSelectedScreenCallback, addConnectedCallback } from "src/app/appstate";
import { config } from "src/app/config";
import { newBoard } from "src/app/gamestate";
import { getBoard } from "../network/network";

export default class Menu extends Phaser.State {
  public init(): void {
    this.game.scale.scaleMode = Phaser.ScaleManager.SHOW_ALL;
    this.game.scale.maxWidth = config.gameWidth;
    this.game.scale.maxHeight = config.gameHeight;
    this.game.scale.pageAlignHorizontally = true;
    this.game.scale.pageAlignVertically = true;

    this.game.canvas.oncontextmenu = e => e.preventDefault();
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const top1: Phaser.Text = this.game.add.text(0, 0, "Home", {
      font: "15px Indie Flower",
      fill: "#F08080",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top1.setTextBounds(20, 10, 40, 20);
    top1.inputEnabled = true;
    top1.events.onInputDown.add(() => changeSelectedScreen("Home"));

    const top2: Phaser.Text = this.game.add.text(0, 0, "Play", {
      font: "15px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    top2.setTextBounds(70, 10, 40, 20);
    top2.inputEnabled = true;
    top2.events.onInputDown.add(() => changeSelectedScreen("Play"));

    const connectionIndicator: Phaser.Text = this.game.add.text(0, 0, "No Connection", {
      font: "15px Indie Flower",
      fill: "#77BFA3",
      boundsAlignH: "center",
      boundsAlignV: "middle",
    });
    connectionIndicator.setTextBounds(690, 10, 90, 20);

    // Play Screen

    const playGroup = this.game.add.group();
    playGroup.visible = false;

    const board = newBoard(this.game, playGroup);

    // Menu Screen

    const menuGroup = this.game.add.group();

    for (const i of [1, 2, 3]) {
      const levelBtn: Phaser.Text = this.game.add.text(0, 0, `level ${i}`, {
        font: "20px Indie Flower",
        fill: "#77BFA3",
        boundsAlignH: "left",
        boundsAlignV: "middle",
      }, menuGroup);
      levelBtn.setTextBounds(75, 30 + i * 30, 75, 50);
      levelBtn.inputEnabled = true;
      levelBtn.events.onInputDown.add(() => {
        getBoard(<any>undefined, board, `${i}`);
      });
    }

    // callbacks

    // callbacks - general

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

    getBoard(<any>undefined, board, "1");
  }

  public update() {
    switch (getSelectedScreen()) {
      case "Play": {
        // gameUpdate(this.game);
        break;
      }
      case "Home": {
        break;
      }
    }
  }
}