import { GSprite } from "./gsprite";

export type PoolInfo = {
  spritesheet: string,
}

export class Pool<A> extends Phaser.Group {
  poolInfo: PoolInfo

  constructor(
    game: Phaser.Game,
    poolInfo: PoolInfo,
    parent?: PIXI.DisplayObjectContainer,
    name?: string,
    addToStage?: boolean,
    enableBody?: boolean,
    physicsBodyType?: number,
  ) {
    super(game, parent, name, addToStage, enableBody, physicsBodyType);
    this.poolInfo = poolInfo;
  }

  newSprite(
    x: number,
    y: number,
    frame: number,
    a: A,
  ): GSprite<A> {
    const sprite: GSprite<A> = this.getFirstExists(false, true, x, y);
    // replace data variable
    sprite.data = a;
    // load texture if it is not loaded yet
    if (sprite.key === "" || sprite.key === null || sprite.key === undefined) {
      sprite.loadTexture(this.poolInfo.spritesheet, frame);
    // otherwise set to correct frame
    } else {
      sprite.frame = frame;
    }
    // initialize sprite if not initialized yet
    if (sprite.props === undefined || sprite.props.init === false) {
      sprite.props = {
        init: false,
      };
      sprite.inputEnabled = true;
      
      sprite.events.onInputOver.add(() => { sprite.frame = 1 });
      sprite.events.onInputOut.add(() => { sprite.frame = 0 });

      sprite.props.init = true;
    }
    return sprite;
  }
}