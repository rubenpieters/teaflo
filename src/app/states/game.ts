import { ActScreen } from "../screens/act/screen";
import { BgScreen } from "../screens/bg/screen";
import { MenuScreen } from "../screens/menu/screen";
import { settings } from "../data/settings";
import { createPosition } from "../util/position";
import { ExecScreen } from "../screens/exec/screen";
import { CodexScreen } from "../screens/codex/screen";
import { SettingsScreen } from "../screens/settings/screen";
import { SaveData, emptySaveData } from "../data/saveData";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    bgScreen: BgScreen,
    actScreen: ActScreen,
    execScreen: ExecScreen,
    menuScreen: MenuScreen,
    codexScreen: CodexScreen,
    settingsScreen: SettingsScreen,
  },
  saveData: SaveData,
  filters: {
    [K in string]: Phaser.Filter
  }
}

let gameRefs: GameRefs;

export default class Game extends Phaser.State {
  public preload(): void {
    this.game.time.advancedTiming = true;
  }

  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    // create filters
    const blueGlow = new Phaser.Filter(this.game, {},
      this.game.cache.getShader("blue-glow")
    );

    // TODO: should screens be part of gameRefs?
    gameRefs = {
      game: this.game,
      screens: {
        bgScreen: <any>undefined,
        actScreen: <any>undefined,
        menuScreen: <any>undefined,
        execScreen: <any>undefined,
        codexScreen: <any>undefined,
        settingsScreen: <any>undefined,
      },
      saveData: emptySaveData(),
      filters: {
        "blue-glow": blueGlow,
      },
    }

    const bgScreen = new BgScreen(gameRefs);
    gameRefs.screens.bgScreen = bgScreen;
    const actScreen = new ActScreen(gameRefs);
    gameRefs.screens.actScreen = actScreen;
    const execScreen = new ExecScreen(gameRefs);
    gameRefs.screens.execScreen = execScreen;
    const menuScreen = new MenuScreen(gameRefs);
    gameRefs.screens.menuScreen = menuScreen;
    const codexScreen = new CodexScreen(gameRefs);
    gameRefs.screens.codexScreen = codexScreen;
    const settingsScreen = new SettingsScreen(gameRefs);
    gameRefs.screens.settingsScreen = settingsScreen;

    bgScreen.initialize();
    actScreen.drawActBtn();
    menuScreen.drawMenuBtn();
  }

  public update(): void {
    for (let key in gameRefs.filters) {
      gameRefs.filters[key].update();
    }
  }

  public render(): void {
    const pos = createPosition(
      "right", 70, settings.gameWidth,
      "top", 20, settings.gameHeight,
    );
    this.game.debug.text(`${this.game.time.fps} FPS` || '--', pos.xMax, pos.yMin, "#00ff00");
    const pos2 = createPosition(
      "right", 140, settings.gameWidth,
      "top", 40, settings.gameHeight,
    );
    this.game.debug.text(`x: ${this.game.input.x} y: ${this.game.input.y}`, pos2.xMax, pos2.yMin);
    //this.game.debug.text(`${gameRefs.screens.levelScreen.buildCardPool.countLiving()}` || '--', pos2.xMax, pos2.yMin, "#00ff00");
  }
}