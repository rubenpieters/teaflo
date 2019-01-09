import { actData } from "../../screens/act/data";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { settings } from "../../data/settings";
import { createPosition } from "../../util/position";

export class ActScreen {
  actBtnPool: Pool<ActBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly game: Phaser.Game
  ) {
    this.actBtnPool = mkActBtnPool(game);
  }

  clear() {
    this.actBtnPool.killAll();
  }

  draw() {
    this.redraw();
    this.actBtnPool.playIntroAnimations();
  }

  redraw() {
    let i = 0;
    for (const actKey in actData) {
      const actId = Number(actKey);

      const pos = createPosition(
        "left", 100 + 500 * i, 400,
        "bot", -100, 200,
      );
      
      this.actBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { actId, actIndex: i });
      i += 1;
    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.actBtnPool.visible = visibility;
  }
}

type ActBtnData = {
  actId: number,
  actIndex: number,
}

function mkActBtnPool(
  game: Phaser.Game,
): Pool<ActBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    game,
    {
      spritesheet: "btn",
      toFrame: frameType => {
        switch (frameType) {
          case "down": return 0;
          case "hover": return 2;
          case "neutral": return 1;
        }
      },
      introAnim: (self, tween) => {
        return tween.from({ y: settings.gameHeight + 400 }, 250, Phaser.Easing.Linear.None, false, 50 * self.data.actIndex);
      },
      callbacks: {},
    },
    self => { return false; }
  );
}