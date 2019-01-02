import { Pool } from "./pool";

export class GSprite<A> extends Phaser.Sprite {
  data: A
  props?: {
    init: boolean,
  }

  constructor(
    game: Phaser.Game,
    x: number,
    y: number,
    data: A,
    key?: string | Phaser.RenderTexture | Phaser.BitmapData | PIXI.Texture,
    frame?: string | number,
  ) {
    super(game, x, y, key, frame);
    this.data = data;
  }
}