import { DataSprite } from "./datasprite";

// FrameType:
// custom type to characterize different frames
// for example: union of string for different button states
export type PoolInfo<Data, FrameType> = {
  // sprites in this pool share this spritesheet
  spritesheet: string,
  // conversion of custom frame type to actual frame index
  toFrame: (frameType: FrameType) => number,
  // intro animation, represented as a Phaser Tween
  introAnim: (sprite: DataSprite<Data>) => Phaser.Tween,
}

export type SpriteCallbacks = {
  onDown?: () => void,
  click?: () => void,
  hoverOut?: () => void,
  hoverOver?: () => void,
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
  newSprite(
    x: number,
    y: number,
    frameType: FrameType,
    data: Data,
    callbacks: {
      onDown: () => void,
      click: () => void,
      hoverOut?: () => void,
      hoverOver?: () => void,
    },
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
          callbacks.onDown();
        }
      });
      sprite.events.onInputUp.removeAll();
      sprite.events.onInputUp.add(() => {
        if (sprite.props !== undefined) {
          const pX = this.game.input.activePointer.x;
          const pY = this.game.input.activePointer.y;
          if (pX >= x && pX <= x + sprite.width && pY >= y && pY <= y + sprite.height) {
            callbacks.click();
          }
          sprite.props.selecting = false;
        }
      });
      sprite.events.onInputOver.removeAll();
      if (callbacks.hoverOver !== undefined) {
        sprite.events.onInputOver.add(callbacks.hoverOver);
      }
      sprite.events.onInputOut.removeAll();
      if (callbacks.hoverOut !== undefined) {
        sprite.events.onInputOut.add(callbacks.hoverOut);
      }

      sprite.props = {
        init: true,
        selecting: false,
      };
    }
    return sprite;
  }

  // NOTE: only give a sprite belonging to this pool as parameter
  setFrame(
    sprite: DataSprite<Data>,
    frameType: FrameType,
  ) {
    sprite.frame = this.poolInfo.toFrame(frameType);
  }

  // play the intro animation of all existing sprites in this pool
  playIntroAnimations() {
    this.forEachExists((sprite: DataSprite<Data>) => {
      const tween = this.poolInfo.introAnim(sprite);
      tween.start();
    });
  }
}

export function newButton<Data>(
  pool: Pool<Data, "neutral" | "hover" | "down">,
  x: number,
  y: number,
  buttonStatus: "neutral" | "hover" | "down",
  data: Data,
  callbacks: SpriteCallbacks,
): DataSprite<Data> {
  const sprite = pool.newSprite(
    x, y, buttonStatus, data,
    {
      onDown: () => {
        pool.setFrame(sprite, "down");
        if (callbacks.onDown !== undefined) callbacks.onDown();
      },
      click: () => {
        pool.setFrame(sprite, "neutral");
        if (callbacks.click !== undefined) callbacks.click();
      },
      hoverOver: () => {
        if (sprite.props !== undefined && sprite.props.selecting) {
          pool.setFrame(sprite, "down");
        } else {
          pool.setFrame(sprite, "hover");
        }
        if (callbacks.hoverOver !== undefined) callbacks.hoverOver();
      },
      hoverOut: () => {
        pool.setFrame(sprite, "neutral");
        if (callbacks.hoverOut !== undefined) callbacks.hoverOut();
      }
    },
  )
  return sprite;
}