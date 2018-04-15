import "p2";
import "pixi";
import "phaser";

import Game from "./states/game";

class App extends Phaser.Game {
  constructor(config: Phaser.IGameConfig) {
    super(config);

    this.state.add("game", Game);

    this.state.start("game");
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
