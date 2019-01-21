import { ActScreen } from "../screens/act/screen";
import { ActSaveData, mkActSaveData } from "../screens/act/data";
import { BgScreen } from "../screens/bg/screen";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    bgScreen: BgScreen,
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
        bgScreen: <any>undefined,
        actScreen: <any>undefined,
      },
      saveData: {
        act: mkActSaveData(),
      }
    }

    const bgScreen = new BgScreen(gameRefs);
    gameRefs.screens.bgScreen = bgScreen;
    const actScreen = new ActScreen(gameRefs);
    gameRefs.screens.actScreen = actScreen;

    actScreen.drawActBtn();
  }
}