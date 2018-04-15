import "p2";
import "pixi";
import "phaser";

import Boot from "./states/boot";
import Load from "./states/load";
import Menu from "./states/menu";
import Game from "./states/game";

class App extends Phaser.Game {
  constructor(config: Phaser.IGameConfig) {
    super(config);

    this.state.add("boot", Boot);
    this.state.add("load", Load);
    this.state.add("menu", Menu);
    this.state.add("game", Game);

    this.state.start("boot");
  }
}

function startApp(): void {
  const gameConfig: Phaser.IGameConfig = {
    width: 800,
    height: 600,
    renderer: Phaser.AUTO,
    parent: "",
    resolution: 1
  };

  const app: Phaser.Game = new App(gameConfig);
}

window.onload = () => {
  startApp();
};
