import "p2";
import "pixi";
import "phaser";

import { Settings, initialSettings } from "./data/settings";
import Boot from "./states/boot";
import Load from "./states/load";
import Game from "./states/game";

window.addEventListener("load", () => {
  main();
});

function main(): void {
  const mSettings = localStorage.getItem("ca_saved_settings");
  let settings: Settings;
  if (mSettings !== null) {
    settings = JSON.parse(mSettings) as Settings;
  } else {
    settings = initialSettings;
  }

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

  game.state.start("boot", undefined, undefined, settings);
}