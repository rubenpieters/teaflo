import { DataSprite } from "./datasprite";
import { createTween, createChainedTween } from "./animation";

// FrameType:
// custom type to characterize different frames
// for example: union of string for different button states
export type PoolInfo<Data, FrameType> = {
  // sprites in this pool share this spritesheet
  spritesheet: string,
  // conversion of custom frame type to actual frame index
  toFrame: (frameType: FrameType) => number,
  // intro animation, represented as a Phaser Tween
  introAnim: ((sprite: DataSprite<Data>, tween: Phaser.Tween) => void)[],
  // custom callbacks for sprites in this pool
  callbacks: SpriteCallbacks<Data>,
}

export type SpriteCallbacks<Data> = {
  onDown?: (self: DataSprite<Data>) => void,
  click?: (self: DataSprite<Data>) => void,
  hoverOut?: (self: DataSprite<Data>) => void,
  hoverOver?: (self: DataSprite<Data>) => void,
}

export class Pool<Data, FrameType> extends Phaser.Group {
  poolInfo: PoolInfo<Data, FrameType>

  constructor(
    game: Phaser.Game,
    poolInfo: PoolInfo<Data, FrameType>,
    parent?: PIXI.DisplayObjectContainer,
    name?: string,
    addToStage?: boolean,
    enableBody?: boolean,
    physicsBodyType?: number,
  ) {
    super(game, parent, name, addToStage, enableBody, physicsBodyType);
    this.poolInfo = poolInfo;
  }

  // create a new sprite in this pool
  // uses getFirstExists to reuse killed sprites
  public newSprite(
    x: number,
    y: number,
    frameType: FrameType,
    data: Data,
  ): DataSprite<Data> {
    const sprite: DataSprite<Data> = this.getFirstExists(false, true, x, y);
    // replace data variable
    sprite.data = data;
    // load texture if it is not loaded yet
    if (sprite.key === "" || sprite.key === null || sprite.key === undefined) {
      sprite.loadTexture(this.poolInfo.spritesheet, this.poolInfo.toFrame(frameType));
    // otherwise set to correct frame
    } else {
      sprite.frame = this.poolInfo.toFrame(frameType);
    }
    // initialize sprite if not initialized yet
    if (sprite.props === undefined || sprite.props.init === false) {
      sprite.inputEnabled = true;
      
      sprite.events.onInputDown.removeAll();
      sprite.events.onInputDown.add(() => {
        if (sprite.props !== undefined) {
          sprite.props.selecting = true;
          invokeIfDefined(this.poolInfo.callbacks.onDown, sprite);
        }
      });
      sprite.events.onInputUp.removeAll();
      sprite.events.onInputUp.add(() => {
        if (sprite.props !== undefined) {
          const pX = this.game.input.activePointer.x;
          const pY = this.game.input.activePointer.y;
          if (pX >= x && pX <= x + sprite.width && pY >= y && pY <= y + sprite.height) {
            invokeIfDefined(this.poolInfo.callbacks.click, sprite);
          }
          sprite.props.selecting = false;
        }
      });
      sprite.events.onInputOver.removeAll();
      sprite.events.onInputOver.add(() => invokeIfDefined(this.poolInfo.callbacks.hoverOver, sprite));
      sprite.events.onInputOut.removeAll();
      sprite.events.onInputOut.add(() => invokeIfDefined(this.poolInfo.callbacks.hoverOut, sprite));

      sprite.props = {
        init: true,
        selecting: false,
      };
    }
    return sprite;
  }

  // NOTE: only give a sprite belonging to this pool as parameter
  public setFrame(
    sprite: DataSprite<Data>,
    frameType: FrameType,
  ) {
    sprite.frame = this.poolInfo.toFrame(frameType);
  }

  // play the intro animation of all existing sprites in this pool
  public playIntroAnimations() {
    this.forEachExists((sprite: DataSprite<Data>) => {
      const fs = this.poolInfo.introAnim.map(f => (x: Phaser.Tween) => f(sprite, x));
      const intro = createChainedTween(this.game, sprite, ...fs);
      if (intro !== undefined) {
        intro.first.start();
      }
    });
  }

  public introTween(
    sprite: DataSprite<Data>,
  ): { first: Phaser.Tween, last: Phaser.Tween } | undefined {
    const fs = this.poolInfo.introAnim.map(f => (x: Phaser.Tween) => f(sprite, x));
    const tween = createChainedTween(this.game, sprite, ...fs);
    return tween;
  }
}

function invokeIfDefined<A>(
  f: ((a: A) => void) | undefined,
  a: A,
): void {
  if (f !== undefined) {
    f(a);
  }
}

export function mkButtonPool<Data>(
  game: Phaser.Game,
  poolInfo: PoolInfo<Data, "neutral" | "hover" | "down">,
  mLockCondition?: (self: DataSprite<Data>) => boolean,
): Pool<Data, "neutral" | "hover" | "down"> {
  const callbacks = poolInfo.callbacks;
  const lockCondition = mLockCondition === undefined ? () => false : mLockCondition;
  const newCallbacks: SpriteCallbacks<Data> = {
    onDown: (self: DataSprite<Data>) => {
      if (! lockCondition(self)) {
        pool.setFrame(self, "down");
        if (callbacks.onDown !== undefined) callbacks.onDown(self);
      }
    },
    click: (self: DataSprite<Data>) => {
      if (! lockCondition(self)) {
        pool.setFrame(self, "hover");
        if (callbacks.click !== undefined) callbacks.click(self);
      }
    },
    hoverOver: (self: DataSprite<Data>) => {
      if (! lockCondition(self)) {
        if (self.props !== undefined && self.props.selecting) {
          pool.setFrame(self, "down");
        } else {
          pool.setFrame(self, "hover");
        }
        if (callbacks.hoverOver !== undefined) callbacks.hoverOver(self);
      }
    },
    hoverOut: (self: DataSprite<Data>) => {
      if (! lockCondition(self)) {
        pool.setFrame(self, "neutral");
        if (callbacks.hoverOut !== undefined) callbacks.hoverOut(self);
      }
    }
  };
  const pool: Pool<Data, "neutral" | "hover" | "down"> = new Pool(
    game, { ...poolInfo, ...{ callbacks: newCallbacks } },
  );
  return pool;
}