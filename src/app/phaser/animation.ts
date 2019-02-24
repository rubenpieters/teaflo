import { DataSprite } from "./datasprite";
import { GameRefs } from "../states/game";
import { Pool } from "./pool";

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

export function createTypeTween(
  gameRefs: GameRefs,
  obj: any,
  f: (tween: Phaser.Tween) => void,
  type?: "log"
): Phaser.Tween {
  const tween = createTween(gameRefs.game, obj, f);
  if (type !== undefined) {
    (<any>tween).data = { };
    (<any>tween).data[type] = true;
    tween.timeScale = speedTypeToSpeed(gameRefs.saveData.act.animationSpeeds[type]);
  }
  return tween;
}

export type SpeedType = "pause" | "play" | "fast" | "skip";

export function speedTypeToSpeed(
  speedType: SpeedType,
): number {
  switch (speedType) {
    case "pause": return 0;
    case "play": return 1;
    case "fast": return 3;
    case "skip": return 1000;
  }
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
        intro.first.start();
      }
    } else {
      chainSpriteCreation(spriteFs.splice(1), animation);
    }
  }
}

export function addTextPopup(
  gameRefs: GameRefs,
  tween: Phaser.Tween,
  createText: () => Phaser.Text,
  textAnimation: (tween: Phaser.Tween) => void,
  type?: "log",
): void {
  tween.onStart.add(() => {
    const text = createText();
    const textTween = createTypeTween(gameRefs, text, textAnimation, type);
    textTween.onComplete.add(() => {
      text.destroy()
    });
    textTween.start();
  });
}

export function addSpritePopup(
  gameRefs: GameRefs,
  tween: Phaser.Tween,
  create: () => Phaser.Sprite,
  textAnimation: (tween: Phaser.Tween) => void,
  type?: "log",
): void {
  tween.onStart.add(() => {
    const sprite = create();
    const spriteTween = createTypeTween(gameRefs, sprite, textAnimation, type);
    spriteTween.onComplete.add(() => {
      sprite.kill();
      sprite.removeChildren();
      // TODO: call .kill() on all children?
    });
    spriteTween.start();
  });
}