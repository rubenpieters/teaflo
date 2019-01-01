import "p2";
import "pixi";
import "phaser";

import { config } from "./config";
import Boot from "./states/boot";
import Load from "./states/load";
import Game from "./states/game";


class App extends Phaser.Game {
  constructor(config: Phaser.IGameConfig) {
    super(config);

    this.state.add("boot", Boot);
    this.state.add("load", Load);
    this.state.add("game", Game);

    this.state.start("boot");
  }
}

function startApp(): void {
  const gameConfig: Phaser.IGameConfig = {
    width: config.gameWidth,
    height: config.gameHeight,
    renderer: Phaser.AUTO,
    parent: "",
    resolution: 1,
    scaleMode: Phaser.ScaleManager.SHOW_ALL,
  };

  new App(gameConfig);
}

window.addEventListener("load", () => {
  startApp();
});