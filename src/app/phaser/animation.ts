import { DataSprite } from "./datasprite";
import { GameRefs } from "../states/game";
import { Pool } from "./pool";

export class Create {
  constructor(
    public readonly f: () => any,
    public readonly k: (self: any) => Animation,
    public readonly tag: "Create" = "Create",
  ) {}
}

export class BaseAnimation {
  constructor(
    public readonly length: number,
    public readonly self: any,
    public readonly f: (tween: Phaser.Tween) => void,
    public readonly tag: "BaseAnimation" = "BaseAnimation",
  ) {}
}

export class SeqAnimation {
  constructor(
    public readonly list: Animation[],
    public readonly tag: "SeqAnimation" = "SeqAnimation",
  ) {}
}

export class ParAnimation {
  constructor(
    public readonly list: Animation[],
    public readonly tag: "ParAnimation" = "ParAnimation",
  ) {}
}

export type Animation
  = BaseAnimation
  | Create
  | SeqAnimation
  | ParAnimation
  ;

export function runAsTween(
  gameRefs: GameRefs,
  animation: Animation,
  type?: "log",
): void {
  _runAsTween(gameRefs, animation, undefined, type);
}

function _runAsTween(
  gameRefs: GameRefs,
  animation: Animation,
  onComplete: (() => void) | undefined,
  type?: "log",
): void {
  switch (animation.tag) {
    case "Create": {
      const obj = animation.f();
      _runAsTween(gameRefs, animation.k(obj), onComplete, type);
      break;
    }
    case "BaseAnimation": {
      const tween = createTypeTween(gameRefs, animation.self, t => animation.f(t), type);
      if (onComplete !== undefined) {
        tween.onComplete.add(onComplete);
      }
      tween.start();
      break;
    }
    case "ParAnimation": {
      if (animation.list.length > 0) {
        const maxAnimT = maxAnimTime(animation.list);
        let onCompleteSet = false;
        animation.list.forEach(childAnimation => {
          if (! onCompleteSet && animTime(childAnimation) === maxAnimT) {
            _runAsTween(gameRefs, childAnimation, onComplete, type);
            onCompleteSet = true;
          } else {
            _runAsTween(gameRefs, childAnimation, undefined, type);
          }
        });
      }
      break;
    }
    case "SeqAnimation": {
      runSeqAsTween(gameRefs, animation, onComplete, type);
      break;
    }
  }
}

export function runCreateOnly(
  animation: Animation,
): void {
  switch (animation.tag) {
    case "Create": {
      const obj = animation.f();
      runCreateOnly(animation.k(obj));
      break;
    }
    case "BaseAnimation": break;
    case "SeqAnimation": // fallthrough
    case "ParAnimation": {
      animation.list.forEach(childAnimation => {
        runCreateOnly(childAnimation);
      });
      break;
    }
  }
}

function runSeqAsTween(
  gameRefs: GameRefs,
  seqAnimation: SeqAnimation,
  onComplete: (() => void) | undefined,
  type?: "log",
) {
  const list = seqAnimation.list;
  if (list.length !== 0) {
    _runAsTween(gameRefs, list[0], () => {
      runSeqAsTween(gameRefs, new SeqAnimation(list.slice(1)), onComplete, type);
    }, type);
  } else {
    if (onComplete !== undefined) {
      onComplete();
    }
  }
}

function animTime(
  animation: Animation,
): number {
  switch (animation.tag) {
    case "Create": return animTime(animation.k(undefined));
    case "BaseAnimation": return animation.length;
    case "SeqAnimation": return animation.list.reduce((acc, prev) => animTime(prev) + acc, 0);
    case "ParAnimation": return maxAnimTime(animation.list);
  }
}

function maxAnimTime(
  animations: Animation[],
): number {
  return Math.max(...animations.map(animTime));
}

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
    const group = create();
    const groupTween = createTypeTween(gameRefs, group, textAnimation, type);
    groupTween.onComplete.add(() => {
      group.destroy();
    });
    groupTween.start();
  });
}