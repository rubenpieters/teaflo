import { ActScreen } from "../screens/act/screen";

export type GameRefs = {
  game: Phaser.Game,
  screens: {
    actScreen: ActScreen,
  }
}

export default class Game extends Phaser.State {
  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const actScreen = new ActScreen(this.game);

    const gameRefs: GameRefs = {
      game: this.game,
      screens: {
        actScreen,
      }
    }

    actScreen.draw();
  }
}