import { DataSprite } from "./datasprite";
import { createTween, createChainedTween } from "./animation";
import { Group } from "phaser-ce";

// FrameType:
// custom type to characterize different frames
// for example: union of string for different button states
export type PoolInfo<Data, FrameType> = {
  // sprites in this pool share this spritesheet
  atlas: string,
  // conversion of custom frame type to actual frame index
  toFrame: (sprite: DataSprite<Data>, frameType: FrameType) => string,
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
  // pool information shared by each element in this pool
  poolInfo: PoolInfo<Data, FrameType>
  // currently registered groups in the pool
  groups: (Phaser.Sprite | undefined)[] = []

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
      sprite.loadTexture(this.poolInfo.atlas, this.poolInfo.toFrame(sprite, frameType));
    // otherwise set to correct frame
    } else {
      const frameName = this.poolInfo.toFrame(sprite, frameType);
      if (frameName !== undefined) {
        sprite.frameName = frameName;
      }
    }
    sprite.inputEnabled = true;
    // initialize sprite if not initialized yet
    if (sprite.props === undefined || sprite.props.init === false) {
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
          if (
            pX >= sprite.x
            && pX <= sprite.x + sprite.width
            && pY >= sprite.y
            && pY <= sprite.y + sprite.height
          ) {
            invokeIfDefined(this.poolInfo.callbacks.click, sprite);
          }
          sprite.props.selecting = false;
        }
      });
      sprite.events.onInputOver.removeAll();
      sprite.events.onInputOver.add((obj: DataSprite<Data>, pointer: Phaser.Pointer) => {
        if (obj.props !== undefined) {
          obj.props.hovering = true;
        }
        invokeIfDefined(this.poolInfo.callbacks.hoverOver, sprite);
      });
      sprite.events.onInputOut.removeAll();
      sprite.events.onInputOut.add((obj: DataSprite<Data>, pointer: Phaser.Pointer) => {
        if (obj.props !== undefined && obj.props.hovering) {
          invokeIfDefined(this.poolInfo.callbacks.hoverOut, sprite);
          obj.props.hovering = false;
        }
      });

      sprite.props = {
        init: true,
        selecting: false,
        hovering: false,
      };
    }
    return sprite;
  }

  public newGroup(
    parentX: number,
    parentY: number,
    dataList: {
      x: number,
      y: number,
      frameType: FrameType,
      data: Data,
    }[]
  ): Phaser.Sprite {
    const group = this.game.add.sprite(parentX, parentY);

    dataList.forEach(data => {
      const sprite = this.newSprite(data.x, data.y, data.frameType, data.data);
      group.addChild(sprite);
    });

    // TODO: check whether this constantly growing list is a problem
    const index = this.groups.length;
    this.groups.push(group);

    group.events.onDestroy.add(() => {
      // remove self from groups
      this.groups[index] = undefined;
      // free the children into the general pool
      group.children.forEach(child => {
        (child as DataSprite<Data>).kill();
        this.add(child);
      });
    });

    return group;
  }

  // NOTE: only give a sprite belonging to this pool as parameter
  public setFrame(
    sprite: DataSprite<Data>,
    frameType: FrameType,
  ) {
    sprite.frameName = this.poolInfo.toFrame(sprite, frameType);
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

  public clear() {
    this.groups.forEach(x => {
      if (x !== undefined) {
        x.destroy();
      }
    });
    // kill all sprites
    this.killAll();
  }

  // clear the animations of the groups in this pool
  public clearGroupAnimations() {
    this.groups.forEach(group => {
      if (group !== undefined) {
        this.game.tweens.removeFrom(group);
        group.destroy();
      }
    });
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