import { ActScreen } from "../screens/act/screen";
import { BgScreen } from "../screens/bg/screen";
import { MenuScreen } from "../screens/menu/screen";
import { Settings } from "../data/settings";
import { createPosition } from "../util/position";
import { ExecScreen } from "../screens/exec/screen";
import { CodexScreen } from "../screens/codex/screen";
import { SettingsScreen } from "../screens/settings/screen";
import { SaveData, emptySaveData } from "../data/saveData";
import { transitionScreen, ScreenAct } from "../screens/transition";

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
  },
  settings: Settings,
}

let gameRefs: GameRefs;

export default class Game extends Phaser.State {
  settings: Settings = undefined as any;

  public preload(): void {
    this.game.time.advancedTiming = true;
  }

  public init(settings: Settings): void {
    this.settings = settings;
  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    // create filters
    const blueGlow = new Phaser.Filter(this.game, {},
      this.game.cache.getShader("blue-glow")
    );
    const blueFlame = new Phaser.Filter(this.game, {},
      this.game.cache.getShader("blue-flame")
    );

    // TODO: should screens be part of gameRefs?
    gameRefs = {
      game: this.game,
      screens: {
        bgScreen: undefined as any,
        actScreen: undefined as any,
        menuScreen: undefined as any,
        execScreen: undefined as any,
        codexScreen: undefined as any,
        settingsScreen: undefined as any,
      },
      saveData: emptySaveData(),
      filters: {
        "blue-glow": blueGlow,
        "blue-flame": blueFlame,
      },
      settings: this.settings,
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
    transitionScreen(gameRefs, new ScreenAct(0));
  }

  public update(): void {
    for (let key in gameRefs.filters) {
      gameRefs.filters[key].update();
    }
  }

  public render(): void {
    if (gameRefs.settings.devMode) {
      const pos = createPosition(this.settings,
        "right", 70, this.settings.gameWidth,
        "top", 20, this.settings.gameHeight,
      );
      this.game.debug.text(`${this.game.time.fps} FPS` || '--', pos.xMax, pos.yMin, "#00ff00");
      const pos2 = createPosition(this.settings,
        "right", 140, this.settings.gameWidth,
        "top", 40, this.settings.gameHeight,
      );
      this.game.debug.text(`x: ${this.game.input.x} y: ${this.game.input.y}`, pos2.xMax, pos2.yMin);
      //this.game.debug.text(`${gameRefs.screens.levelScreen.buildCardPool.countLiving()}` || '--', pos2.xMax, pos2.yMin, "#00ff00");
    }
  }
}