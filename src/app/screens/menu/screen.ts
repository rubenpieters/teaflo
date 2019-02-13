import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createTween, chainSpriteCreation } from "../../phaser/animation";
import { settings } from "../../data/settings";
import { loadActScreen, loadExecScreen, loadCodexScreen, loadSettingsScreen } from "./events";
import { createPosition } from "../../util/position";
import { drawCurrentLevel } from "../exec/events";
import { DataSprite } from "src/app/phaser/datasprite";

export class MenuScreen {
  menuBgPool: Pool<MenuBgData, {}>
  menuBtnPool: Pool<MenuBtnData, "neutral" | "hover" | "down">

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


    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[];

    const bgF = {
      create: () => {
        const pos = createPosition(
          "right", 0, 300,
          "top", 0, 1080,
        );
        const sprite = this.menuBgPool.newSprite(pos.xMin, pos.yMin, {}, {});
        return sprite;
      },
      introTween: (sprite: DataSprite<MenuBgData>) => {
        return this.menuBgPool.introTween(sprite);
      },
    };

    let i = 0;
    // buttons are placed in reverse order on the screen
    const l: ["settings", "codex", "schem", "menu"] = ["settings", "codex", "schem", "menu"];
    const buttonF = l.map((type, i) => {
      return {
        create: () => {
          let sprite;
          const pos = createPosition(
            "right", 335, 70,
            "top", 200 * i, 70,
          );
          if (this.gameRefs.saveData.act.activeScreen === type) {
            // this is the currently selected menu type
            sprite = this.menuBtnPool.newSprite(pos.xMin, pos.yMin, "down", { type, index: i, });
          } else {
            // this is not the currently selected act
            sprite = this.menuBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { type, index: i, });
          }
          return sprite;
        },
        introTween: (sprite: DataSprite<MenuBtnData>) => {
          return this.menuBtnPool.introTween(sprite);
        },
      }
    });

    spriteFs = [bgF];
    spriteFs = spriteFs.concat(buttonF);
    chainSpriteCreation(spriteFs, animation);
  }
}

type MenuBtnData = {
  type: "menu" | "schem" | "codex" | "settings",
  index: number,
}

function mkMenuBtnPool(
  gameRefs: GameRefs,
): Pool<MenuBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return "menu_btn2.png";
          case "hover": return "menu_btn3.png";
          case "neutral": return "menu_btn1.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ alpha: 0 }, 30, Phaser.Easing.Linear.None, false, 0);
        },
      ],
      callbacks: {
        click: (self) => {
          switch (self.data.type) {
            case "menu": {
              gameRefs.screens.execScreen.clearAnimations();
              loadActScreen(gameRefs);
              return;
            }
            case "schem": {
              gameRefs.screens.execScreen.clearAnimations();
              drawCurrentLevel(gameRefs);
              return;
            }
            case "codex": {
              gameRefs.screens.execScreen.clearAnimations();
              loadCodexScreen(gameRefs);
              return;
            }
            case "settings": {
              gameRefs.screens.execScreen.clearAnimations();
              loadSettingsScreen(gameRefs);
              return;
            }
          }
        },
        /*hoverOver: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: settings.gameHeight - 300 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },
        hoverOut: (self) => {
          const tween = createTween(gameRefs.game, self, x => x.to({ y: settings.gameHeight - 200 }, 75, Phaser.Easing.Linear.None, false, 0));
          tween.start();
        },*/
      },
    },
    self => {
      return gameRefs.saveData.act.activeScreen === self.data.type
        || self.data.type === "schem" && gameRefs.saveData.act.currentSchem === undefined
      ;
    }
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