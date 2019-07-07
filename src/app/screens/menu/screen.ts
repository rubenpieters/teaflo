import { Pool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { chainSpriteCreation, Create, SeqAnimation, runAsTween, runCreateOnly, BaseAnimation } from "../../phaser/animation";
import { createPosition } from "../../util/position";
import { drawCurrentLevel } from "../exec/events";
import { DataSprite, clearShader, addShader } from "../../phaser/datasprite";
import { loadActMenu } from "../act/events";
import { transitionScreen, ScreenCodex, ScreenSettings } from "../transition";
import { settings } from "../../data/settings";

export class MenuScreen {
  menuBgPool: Pool<MenuBgData, {}>
  menuBtnPool: Pool<MenuBtnData, "selected" | "not_selected">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.menuBgPool = mkMenuBgPool(gameRefs);
    this.menuBtnPool = mkMenuBtnPool(gameRefs);
  }

  drawMenuBtn() {
    this.redrawMenuBtn(true);
    this.menuBtnPool.playIntroAnimations();
  }

  redrawMenuBtn(
    animation: boolean = false,
  ) {
    this.menuBgPool.clear();
    this.menuBtnPool.clear();

    // buttons are placed in reverse order on the screen
    const l: ["settings", "codex", "schem", "menu"] = ["settings", "codex", "schem", "menu"];
    const anims = l.map((type, i) => {
      return new Create(
        () => {
          let sprite;
          const pos = createPosition(
            "right", (120 * i), 100,
            "bot", 20, 100,
          );
          if (this.gameRefs.saveData.act.activeScreen === type) {
            // this is the currently selected menu type
            sprite = this.menuBtnPool.newSprite(pos.xMin, pos.yMin, "not_selected", { type, index: i, originalX: pos.xMin });
          } else {
            // this is not the currently selected act
            sprite = this.menuBtnPool.newSprite(pos.xMin, pos.yMin, "selected", { type, index: i, originalX: pos.xMin });
          }
          return sprite;
        },
        (self) => {
          return new BaseAnimation(
            50,
            self,
            (tween) => {
              tween.from({ x: self.x - 50, alpha: 0 }, 50, Phaser.Easing.Linear.None, false, 0);
            },
          );
        }
      )
    });

    if (animation) {
      runAsTween(this.gameRefs, new SeqAnimation(anims));
    } else {
      runCreateOnly(new SeqAnimation(anims));
    }
  }
}

type MenuBtnData = {
  type: "menu" | "schem" | "codex" | "settings",
  index: number,
  originalX: number,
}

function mkMenuBtnPool(
  gameRefs: GameRefs,
): Pool<MenuBtnData, "selected" | "not_selected"> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.type) {
          case "menu": return `menu_select_100_100.png`;
          case "schem": return `menu_schem_100_100.png`;
          case "codex": return `menu_codex_100_100.png`;
          case "settings": return `menu_options_100_100.png`;
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ x: self.x - 50, alpha: 0.5 }, 30, Phaser.Easing.Linear.None, false, 0);
        },
      ],
      callbacks: {
        click: (self) => {
          if (! (gameRefs.saveData.act.activeScreen === self.data.type
            || self.data.type === "schem" && gameRefs.saveData.act.currentLevelId === undefined)
          ) {
            switch (self.data.type) {
              case "menu": {
                loadActMenu(gameRefs);
                return;
              }
              case "schem": {
                drawCurrentLevel(gameRefs);
                return;
              }
              case "codex": {
                transitionScreen(gameRefs, new ScreenCodex());
                return;
              }
              case "settings": {
                transitionScreen(gameRefs, new ScreenSettings());
                return;
              }
            }
          }
        },
        hoverOver: (self) => {
          addShader(gameRefs, self, "blue-glow");
        },
        hoverOut: (self) => {
          clearShader(self);
        },
      },
    },
    /*self => {
      return gameRefs.saveData.act.activeScreen === self.data.type
        || self.data.type === "schem" && gameRefs.saveData.act.currentSchem === undefined
      ;
    }*/
  );
}

type MenuBgData = {}

function mkMenuBgPool(
  gameRefs: GameRefs,
): Pool<MenuBgData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => { return "menu_bar.png"; },
      introAnim: [
        (self, tween) => {
          tween.from({ alpha: 0 }, 125, Phaser.Easing.Linear.None, false, 0);
        },
      ],
      callbacks: {},
    },
  );
}