import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "src/app/util/position";
import { addText } from "src/app/phaser/datasprite";

export class LevelScreen {
  clearBtnPool: Pool<{}, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.clearBtnPool = mkClearBtnPool(gameRefs);
  }

  drawClearBtn(
  ) {
    this.redrawExecStartBtn();
    this.clearBtnPool.playIntroAnimations();
  }

  redrawExecStartBtn(
  ) {
    this.clearBtnPool.killAll();

    const pos = createPosition(
      "right", 400, 400,
      "bot", 300, 200,
    );
    const sprite = this.clearBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Clear Solution", "#000000", 40);
  }

}

function mkClearBtnPool(
  gameRefs: GameRefs,
): Pool<{}, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return "btn_level_click.png";
          case "hover": return "btn_level_hover.png";
          case "neutral": return "btn_level_neutral.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 50, Phaser.Easing.Linear.None, false, 0);
        }
      ],
      callbacks: {
        click: (self) => {
          console.log("CLEAR!");
        },
      },
    },
    self => { return false; }
  );
}