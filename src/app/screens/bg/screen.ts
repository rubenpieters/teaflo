import { GameRefs } from "../../states/game";
import { Pool } from "../../phaser/pool";

export class BgScreen {
  bgPool: Pool<{}, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.bgPool = mkBgPool(gameRefs);
  }

  initialize() {
    this.bgPool.killAll();

    const sprite = this.bgPool.newSprite(0, 0, {}, {});
    sprite.loadTexture("bg4");
  }

  drawBg(
  ) {
    this.bgOnIntroComplete(() => { return; });
  }

  bgOnIntroComplete(
    after: () => void,
  ) {
    this.bgPool.killAll();

    const sprite = this.bgPool.newSprite(0, 0, {}, {});
    sprite.loadTexture("bg3");

    const intro = this.bgPool.introTween(sprite);
    if (intro !== undefined) {
      intro.last.onComplete.add(() => after());
      intro.first.start();
    }
  }

  clearAnimations() {
    this.gameRefs.game.tweens.removeFrom(this.bgPool, true);
  }
}

function mkBgPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "bg4",
      toFrame: frameType => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({}, 15);
          tween.onComplete.add(() => self.loadTexture("bg2"));
        },
        (self, tween) => {
          tween.from({}, 15);
          tween.onComplete.add(() => self.loadTexture("bg1"));
        },
        (self, tween) => {
          tween.from({}, 15);
          tween.onComplete.add(() => self.loadTexture("bg0"));
        },
        (self, tween) => {
          tween.from({}, 15);
          tween.onComplete.add(() => {
            self.loadTexture("bg4");
        });
        },
      ],
      callbacks: {},
    },
  )
}

/*

Seq [
  Base (15, onComplete)
  Base (15, onComplete)
  Base (15, onComplete)
  Base (15, onComplete)
]

*/