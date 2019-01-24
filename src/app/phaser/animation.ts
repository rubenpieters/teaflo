import { DataSprite } from "./datasprite";

export function createTween(
  game: Phaser.Game,
  obj: any,
  f: (tween: Phaser.Tween) => void,
): Phaser.Tween {
  let tween = game.add.tween(obj);
  tween.frameBased = true;
  f(tween);
  tween.onComplete.add(() => {
    game.tweens.remove(tween);
  });
  return tween;
}

export function createChainedTween(
  game: Phaser.Game,
  obj: any,
  ...fs: ((tween: Phaser.Tween) => void)[]
): { first: Phaser.Tween, last: Phaser.Tween } | undefined {
  let first: Phaser.Tween | undefined = undefined;
  let last: Phaser.Tween | undefined = undefined;
  fs.forEach(f => {
    let t = game.add.tween(obj);
    t.frameBased = true;
    f(t);
    t.onComplete.add(() => {
      game.tweens.remove(t);
    });
    if (last !== undefined) {
      last.chain(t);
      last = t;
    } else {
      first = t;
      last = t;
    }
  });
  if (first !== undefined && last !== undefined) {
    return { first, last };
  }
  return undefined;
}

export function chainSpriteCreation(
  spriteFs: {
    create: () => DataSprite<any>,
    introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
  }[],
  animation: boolean,
) {
  if (spriteFs.length === 0) {
    return;
  } else {
    const fs = spriteFs[0];
    const sprite = fs.create();
    if (animation) {
      const intro = fs.introTween(sprite);
      if (intro !== undefined) {
        intro.last.onComplete.add(() => {
          chainSpriteCreation(spriteFs.splice(1), animation);
        });
      }
    } else {
      chainSpriteCreation(spriteFs.splice(1), animation);
    }
  }
}