import { Pool } from "../phaser/pool";
import { config } from "../config";

export default class Game extends Phaser.State {
  public init(): void {

  }

  public create(): void {
    this.stage.backgroundColor = 0xDCDCDC;

    const pool: Pool<{}> = new Pool(this.game, { spritesheet: "btn" });
    pool.newSprite(100, config.gameHeight - 100, 0, {});
    pool.newSprite(600, config.gameHeight - 100, 0, {});
    pool.newSprite(1100, config.gameHeight - 100, 0, {});
  }
}