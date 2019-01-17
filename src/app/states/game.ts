import { ActScreen } from "../screens/act/screen";
import { ActSaveData, actSaveData } from "../screens/act/data";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    actScreen: ActScreen,
  },
  saveData: {
    act: ActSaveData,
  }
}

export default class Game extends Phaser.State {
  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    // TODO: should screens be part of gameRefs?
    let gameRefs: GameRefs = {
      game: this.game,
      screens: {
        actScreen: <any>undefined,
      },
      saveData: {
        act: actSaveData,
      }
    }

    const actScreen = new ActScreen(gameRefs);
    gameRefs.screens.actScreen = actScreen;

    actScreen.draw();
  }
}