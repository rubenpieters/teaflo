import { ActScreen } from "../screens/act/screen";
import { ActSaveData, mkActSaveData, LevelSaveData, mkLevelSaveData } from "../screens/act/data";
import { BgScreen } from "../screens/bg/screen";
import { LevelScreen } from "../screens/level/screen";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    bgScreen: BgScreen,
    actScreen: ActScreen,
    levelScreen: LevelScreen,
  },
  saveData: {
    act: ActSaveData,
    level: LevelSaveData,
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
        levelScreen: <any>undefined,
      },
      saveData: {
        act: mkActSaveData(),
        level: mkLevelSaveData(),
      }
    }

    const bgScreen = new BgScreen(gameRefs);
    gameRefs.screens.bgScreen = bgScreen;
    const actScreen = new ActScreen(gameRefs);
    gameRefs.screens.actScreen = actScreen;
    const levelScreen = new LevelScreen(gameRefs);
    gameRefs.screens.levelScreen = levelScreen;

    actScreen.drawActBtn();
  }
}