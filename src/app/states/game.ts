import { Pool, newButton } from "../phaser/pool";
import { config } from "../config";

export default class Game extends Phaser.State {
  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const pool: Pool<{}, "neutral" | "hover" | "down"> = new Pool(this.game,
      {
        spritesheet: "btn",
        toFrame: (frameType) => {
          switch (frameType) {
            case "down": return 0;
            case "hover": return 2;
            case "neutral": return 1;
          }
        }
      }
    );
    newButton(pool, 100, config.gameHeight - 100, "neutral", {}, {});
    newButton(pool, 600, config.gameHeight - 100, "neutral", {}, {});
    newButton(pool, 1100, config.gameHeight - 100, "neutral", {}, {});
  }
}