import { GameRefs } from "../../states/game";
import { Pool } from "../../phaser/pool";

export class LevelSelectScreen {
  bgPool: Pool<{}, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.bgPool = mkBgPool(gameRefs);
  }

  clear() {
    this.bgPool.killAll();
  }

  draw() {
    this.redraw();
    this.bgPool.playIntroAnimations();
  }

  redraw() {
    this.clear();

    const sprite = this.bgPool.newSprite(0, 0, {}, {});
    sprite.loadTexture("bg0");
  }

  setVisibility(
    visibility: boolean
  ) {
    this.bgPool.visible = visibility;
  }
}

function mkBgPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      spritesheet: "bg0",
      toFrame: frameType => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg1"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg2"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg3"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg4"));
        },
      ],
      callbacks: {},
    },
  )
}