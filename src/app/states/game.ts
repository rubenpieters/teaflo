import { Pool, newButton } from "../phaser/pool";
import { config } from "../config";

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
          const introAnim = this.add.tween(self);
          introAnim.frameBased = true;
          introAnim.from({ y: config.gameHeight + 400 }, 50, Phaser.Easing.Linear.None, false, 100 * self.data);
          introAnim.onComplete.add(() => {
            this.tweens.remove(introAnim);
          });
          return introAnim;
        }
      }
    );
    newButton(pool, 100, config.gameHeight - 100, "neutral", 0, {});
    newButton(pool, 600, config.gameHeight - 100, "neutral", 1, {});
    newButton(pool, 1100, config.gameHeight - 100, "neutral", 2, {});
    pool.playIntroAnimations();
  }
}