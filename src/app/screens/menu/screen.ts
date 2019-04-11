import { Pool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { chainSpriteCreation } from "../../phaser/animation";
import { createPosition } from "../../util/position";
import { drawCurrentLevel } from "../exec/events";
import { DataSprite } from "src/app/phaser/datasprite";
import { loadActMenu } from "../act/events";
import { transitionScreen, ScreenCodex, ScreenSettings } from "../transition";

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


    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[];

    const bgF = {
      create: () => {
        const pos = createPosition(
          "right", 0, 150,
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
            "right", 0, 150,
            "top", 140 + 270 * i, 100,
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
        const num = frameType === "selected" ? 2 : 1;
        switch (self.data.type) {
          case "menu": return `select${num}.png`;
          case "schem": return `schem${num}.png`;
          case "codex": return `codex${num}.png`;
          case "settings": return `options${num}.png`;
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
            || self.data.type === "schem" && gameRefs.saveData.act.currentSchem === undefined)
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
          /*if (! (gameRefs.saveData.act.activeScreen === self.data.type
            || self.data.type === "schem" && gameRefs.saveData.act.currentSchem === undefined)
          ) {
            gameRefs.game.tweens.removeFrom(self);
            const tween1 = createTween(gameRefs.game, self, x => x.to({}, 10, Phaser.Easing.Linear.None, false, 0));
            tween1.onComplete.add(() => { self.frameName = "menu_btn_joined1.png"; self.x = self.data.originalX - 50; });
            const tween2 = createTween(gameRefs.game, self, x => x.to({}, 10, Phaser.Easing.Linear.None, false, 0));
            tween2.onComplete.add(() => { self.frameName = "menu_btn_joined2.png"; self.x = self.data.originalX - 100; });
            const tween3 = createTween(gameRefs.game, self, x => x.to({}, 10, Phaser.Easing.Linear.None, false, 0));
            tween3.onComplete.add(() => { self.frameName = "menu_btn_joined3.png"; self.x = self.data.originalX - 150; });
            const tween4 = createTween(gameRefs.game, self, x => x.to({}, 10, Phaser.Easing.Linear.None, false, 0));
            tween4.onComplete.add(() => { self.frameName = "menu_btn_joined4.png"; self.x = self.data.originalX - 200; });
            tween1.chain(tween2, tween3, tween4);
            tween1.start();
          }*/
        },
        hoverOut: (self) => {
          /*if (! (gameRefs.saveData.act.activeScreen === self.data.type
            || self.data.type === "schem" && gameRefs.saveData.act.currentSchem === undefined)
          ) {
            gameRefs.game.tweens.removeFrom(self);
            const tween1 = createTween(gameRefs.game, self, x => x.to({}, 11, Phaser.Easing.Linear.None, false, 0));
            tween1.onComplete.add(() => { self.frameName = "menu_btn_joined3.png"; self.x = self.data.originalX - 150; });
            const tween2 = createTween(gameRefs.game, self, x => x.to({}, 11, Phaser.Easing.Linear.None, false, 0));
            tween2.onComplete.add(() => { self.frameName = "menu_btn_joined2.png"; self.x = self.data.originalX - 100; });
            const tween3 = createTween(gameRefs.game, self, x => x.to({}, 11, Phaser.Easing.Linear.None, false, 0));
            tween3.onComplete.add(() => { self.frameName = "menu_btn_joined1.png"; self.x = self.data.originalX - 50; });
            const tween4 = createTween(gameRefs.game, self, x => x.to({}, 11, Phaser.Easing.Linear.None, false, 0));
            tween4.onComplete.add(() => { self.frameName = "menu_btn_joined.png"; self.x = self.data.originalX; });
            tween1.chain(tween2, tween3, tween4);
            tween1.start();
          }*/
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