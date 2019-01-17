import { actData, selectedActId } from "../../screens/act/data";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { settings } from "../../data/settings";
import { createPosition } from "../../util/position";
import { createTween } from "../../phaser/animation";
import { GameRefs } from "src/app/states/game";
import { changeAct } from "./events";

export class ActScreen {
  actBtnPool: Pool<ActBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.actBtnPool = mkActBtnPool(gameRefs);
  }

  clear() {
    this.actBtnPool.killAll();
  }

  draw() {
    this.redraw();
    this.actBtnPool.playIntroAnimations();
  }

  redraw() {
    this.clear();

    let i = 0;
    for (const actKey in actData) {
      const actId = Number(actKey);
      
      const selActId = selectedActId(this.gameRefs);
      if (selActId !== undefined && selActId == actId) {
        // this is the currently selected act
        const pos = createPosition(
          "left", 100 + 210 * i, 400,
          "top", -100, 200,
        );
        this.actBtnPool.newSprite(pos.xMin, pos.yMin, "down", { actId, actIndex: i, selected: true, });
      } else {
        const pos = createPosition(
          "left", 100 + 210 * i, 400,
          "top", -200, 200,
        );
        // this is not the currently selected act
        this.actBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { actId, actIndex: i, selected: false, });
      }
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
  selected: boolean,
}

function mkActBtnPool(
  gameRefs: GameRefs,
): Pool<ActBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      spritesheet: "btn",
      toFrame: frameType => {
        switch (frameType) {
          case "down": return 1;
          case "hover": return 2;
          case "neutral": return 0;
        }
      },
      introAnim: (self, tween) => {
        return tween.from({ y: -400 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.actIndex);
      },
      callbacks: {
        click: (self) => {
          changeAct(gameRefs, self.data.actId);
        },
        hoverOver: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: -100 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },
        hoverOut: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: -200 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },
      },
    },
    self => { return self.data.selected; }
  );
}