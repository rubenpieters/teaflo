import "p2";
import "pixi";
import "phaser";

import Boot from "src/app/states/boot";
import Load from "src/app/states/load";
import Menu from "src/app/states/menu";
import Game from "src/app/states/game";


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
