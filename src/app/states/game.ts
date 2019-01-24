import { ActScreen } from "../screens/act/screen";
import { ActSaveData, mkActSaveData, LevelSaveData, mkLevelSaveData } from "../screens/act/data";
import { BgScreen } from "../screens/bg/screen";
import { LevelScreen } from "../screens/level/screen";
import { MenuScreen } from "../screens/menu/screen";
import { settings } from "../data/settings";
import { createPosition } from "../util/position";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    bgScreen: BgScreen,
    actScreen: ActScreen,
    levelScreen: LevelScreen,
    menuScreen: MenuScreen,
  },
  saveData: {
    act: ActSaveData,
    level: LevelSaveData,
  }
}

export default class Game extends Phaser.State {
  public preload(): void {
    this.game.time.advancedTiming = true;
  }

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
        menuScreen: <any>undefined,
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
    const menuScreen = new MenuScreen(gameRefs);
    gameRefs.screens.menuScreen = menuScreen;

    actScreen.drawActBtn();
    menuScreen.drawMenuBtn();
  }

  public render(): void {
    const pos = createPosition(
      "right", 70, settings.gameWidth,
      "top", 20, settings.gameHeight,
    );
    this.game.debug.text(`${this.game.time.fps} FPS` || '--', pos.xMax, pos.yMin, "#00ff00");   
  }
}