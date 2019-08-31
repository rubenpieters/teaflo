import { Pool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { chainSpriteCreation, Create, SeqAnimation, runAsTween, runCreateOnly, BaseAnimation, runAnim } from "../../phaser/animation";
import { createPosition } from "../../util/position";
import { drawCurrentLevel } from "../exec/events";
import { DataSprite, clearShader, addShader } from "../../phaser/datasprite";
import { loadActMenu } from "../act/events";
import { transitionScreen, ScreenCodex, ScreenSettings } from "../transition";

export class MenuScreen {
  menuBgPool: Pool<MenuBgData, {}>
  menuBtnPool: Pool<MenuBtnData, "selected" | "not_selected">
  bgSpritePool: Pool<BgSpriteData, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.menuBgPool = mkMenuBgPool(gameRefs);
    this.menuBtnPool = mkMenuBtnPool(gameRefs);
    this.bgSpritePool = mkBgSpritePool(gameRefs);
  }

  drawMenuBtn(animations: boolean = false) {
    this.redrawMenuBtn(animations);
  }

  redrawMenuBtn(
    animations: boolean = false,
  ) {
    this.menuBgPool.clear();
    this.menuBtnPool.clear();

    // buttons are placed in reverse order on the screen
    const l: ["settings", "codex", "exec", "menu"] = ["settings", "codex", "exec", "menu"];
    const anims = l.map((type, i) => {
      return new Create(
        () => {
          let sprite;
          const pos = createPosition(this.gameRefs.settings,
            "right", 15 + (105 * i), 100,
            "bot", 15, 100,
          );
          if (this.gameRefs.saveData.activeScreen === type) {
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
              tween.from({ y: self.y + 30, alpha: 0.5 }, 50, Phaser.Easing.Linear.None, false, 0);
            },
          );
        }
      )
    });

    runAnim(animations, this.gameRefs, new SeqAnimation(anims));
  }

  drawBg() {
    this.bgSpritePool.clear();

    this.bgSpritePool.newSprite(this.gameRefs.settings.gameWidth - 450  , this.gameRefs.settings.gameHeight - 130, {}, { sprite: "menu_450_125.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);

    // for (const i of [0,1,2,3]) {
    //   const pos = createPosition(this.gameRefs.settings,
    //     "right", (120 * i), 120,
    //     "bot", 0, 100,
    //   );
    //   this.bgSpritePool.newSprite(pos.xMin, pos.yMin, {}, { sprite: "arc_130_130.png" }, /*alpha*/ undefined, /*inputEnabled*/ false);
    // }
  }
}

type MenuBtnData = {
  type: "menu" | "exec" | "codex" | "settings",
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
          case "menu": return `menu_select2_100_100.png`;
          case "exec": return `menu_schem2_100_100.png`;
          case "codex": return `menu_codex2_100_100.png`;
          case "settings": return `menu_options2_100_100.png`;
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ x: self.x - 50, alpha: 0.5 }, 30, Phaser.Easing.Linear.None, false, 0);
        },
      ],
      callbacks: {
        click: (self) => {
          if (! (gameRefs.saveData.activeScreen === self.data.type
            || self.data.type === "exec" && gameRefs.saveData.currentLevelId === undefined)
          ) {
            switch (self.data.type) {
              case "menu": {
                loadActMenu(gameRefs);
                return;
              }
              case "exec": {
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

type BgSpriteData = {
  sprite: string,
};

function mkBgSpritePool(
  gameRefs: GameRefs,
): Pool<BgSpriteData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {

        },
      },
    },
  );
}