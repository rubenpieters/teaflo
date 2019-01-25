import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createTween } from "../../phaser/animation";
import { settings } from "../../data/settings";
import { loadActScreen } from "./events";
import { createPosition } from "../../util/position";

export class MenuScreen {
  menuBtnPool: Pool<MenuBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.menuBtnPool = mkMenuBtnPool(gameRefs);
  }

  drawMenuBtn() {
    this.redrawMenuBtn();
    this.menuBtnPool.playIntroAnimations();
  }

  redrawMenuBtn() {
    this.menuBtnPool.killAll();

    let i = 0;
    // buttons are placed in reverse order on the screen
    const l: ("act" | "level")[] = ["level", "act"];
    for (const type of l) {
      
      /*if (selActId !== undefined && selActId === actId) {
        // this is the currently selected act
        const pos = createPosition(
          "left", 100 + 210 * i, 400,
          "top", -100, 200,
        );
        this.actBtnPool.newSprite(pos.xMin, pos.yMin, "down", { actId, actIndex: i, });
      } else {*/
        const pos = createPosition(
          "right", 100 + 210 * i, 200,
          "bot", - 200, 400,
        );
        // this is not the currently selected act
        this.menuBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { type, index: i, });
      //}
      i += 1;
    }
  }
}

type MenuBtnData = {
  type: "act" | "level",
  index: number,
}

function mkMenuBtnPool(
  gameRefs: GameRefs,
): Pool<MenuBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: frameType => {
        switch (frameType) {
          case "down": return "bt_bmark_click.png";
          case "hover": return "bt_bmark_hover.png";
          case "neutral": return "bt_bmark_neutral.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: settings.gameHeight + 200 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.index);
        }
      ],
      callbacks: {
        click: (self) => {
          switch (self.data.type) {
            case "act": {
              loadActScreen(gameRefs);
              break;
            }
            case "level": {
              throw "TODO";
            }
          }
        },
        hoverOver: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: settings.gameHeight - 300 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },
        hoverOut: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: settings.gameHeight - 200 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },
      },
    },
    self => { return false; }
  );
}