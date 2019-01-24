import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText } from "../../phaser/datasprite";

export class LevelScreen {
  boxPool: Pool<{}, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.boxPool = mkBoxPool(gameRefs);
  }

  drawBox(
  ) {
    this.boxPool.killAll();
    this.gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => {
        this.createBox(true);
      },
    );
  }

  createBox(
    animation: boolean,
  ) {
    const pos = createPosition(
      "left", 0, 640,
      "top", 0, 1080,
    );
    const sprite = this.boxPool.newSprite(pos.xMin, pos.yMin, {}, {});
    if (animation) {
      const intro = this.boxPool.introTween(sprite);
      if (intro !== undefined) {
        intro.first.start();
      }
    } else {

    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.boxPool.visible = visibility;
  }
}

function mkBoxPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      spritesheet: "box",
      toFrame: frameType => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({ x: self.x - 640 }, 75, Phaser.Easing.Linear.None, false, 50);
        },
      ],
      callbacks: {},
    },
  )
}

function mkCard(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      spritesheet: "fr_unit_a1_l2_01",
      toFrame: frameType => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({ x: self.x - 50 }, 75, Phaser.Easing.Linear.None, false, 50);
        },
      ],
      callbacks: {},
    },
  )
}
