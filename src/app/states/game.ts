import { ActScreen } from "../screens/act/screen";
import { ActSaveData, mkActSaveData } from "../screens/act/data";
import { LevelSelectScreen } from "../screens/levelselect/screen";
import { LevelSelectSaveData, mkLevelSaveData } from "../screens/levelselect/data";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    actScreen: ActScreen,
    levelSelectScreen: LevelSelectScreen,
  },
  saveData: {
    act: ActSaveData,
    levelSelect: LevelSelectSaveData,
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
        levelSelectScreen: <any>undefined,
      },
      saveData: {
        act: mkActSaveData(),
        levelSelect: mkLevelSaveData(),
      }
    }

    const levelSelectScreen = new LevelSelectScreen(gameRefs);
    gameRefs.screens.levelSelectScreen = levelSelectScreen;
    const actScreen = new ActScreen(gameRefs);
    gameRefs.screens.actScreen = actScreen;

    actScreen.draw();
    levelSelectScreen.draw();
  }
}