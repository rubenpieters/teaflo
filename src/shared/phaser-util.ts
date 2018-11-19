import { Overwrite } from "./type-util";

export function intersects(
  bounds1: PIXI.Rectangle,
  bounds2: PIXI.Rectangle,
) {
  // this intersects method seems to work on PIXI Rectangles as well
  return Phaser.Rectangle.intersects(<any>bounds1, <any>bounds2);
}

export type GSprite<A> = Overwrite<Phaser.Sprite, { data: A }>;
