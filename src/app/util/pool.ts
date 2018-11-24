export class SpritePool<A> extends Phaser.Group {
  getFirstExists(
    exists: boolean,
    createIfNull?: boolean,
    x?: number,
    y?: number,
    key?: string | Phaser.RenderTexture | Phaser.BitmapData | Phaser.Video | PIXI.Texture,
    frame?: string | number,
  ): A {
    return super.getFirstExists(exists, createIfNull, x, y, key, frame);
  }
}
