import "p2";
import "pixi";
import "phaser";

import { settings } from "./data/settings";
import Boot from "./states/boot";
import Load from "./states/load";
import Game from "./states/game";

window.addEventListener("load", () => {
  main();
});

function main(): void {
  const gameConfig: Phaser.IGameConfig = {
    width: settings.gameWidth,
    height: settings.gameHeight,
    renderer: Phaser.WEBGL,
    parent: "",
    resolution: 1,
    scaleMode: Phaser.ScaleManager.SHOW_ALL,
  };

  const game = new Phaser.Game(gameConfig);

  game.state.add("boot", Boot);
  game.state.add("load", Load);
  game.state.add("game", Game);

  game.state.start("boot");
}