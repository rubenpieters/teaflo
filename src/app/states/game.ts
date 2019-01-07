import { Pool, newButton } from "../phaser/pool";
import { settings } from "../data/settings";
import { createTween } from "../phaser/animation";

export default class Game extends Phaser.State {
  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const pool: Pool<number, "neutral" | "hover" | "down"> = new Pool(this.game,
      {
        spritesheet: "btn",
        toFrame: (frameType) => {
          switch (frameType) {
            case "down": return 0;
            case "hover": return 2;
            case "neutral": return 1;
          }
        },
        introAnim: (self) => {
          return createTween(this.game, self,
            tween => tween.from({ y: settings.gameHeight + 400 }, 50, Phaser.Easing.Linear.None, false, 100 * self.data)
          );
        }
      }
    );
    newButton(pool, 100, settings.gameHeight - 100, "neutral", 0, {});
    newButton(pool, 600, settings.gameHeight - 100, "neutral", 1, {});
    newButton(pool, 1100, settings.gameHeight - 100, "neutral", 2, {});
    pool.playIntroAnimations();
  }
}