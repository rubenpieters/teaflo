import { Pool, mkButtonPool } from "../../phaser/pool";
import { createPosition, Position } from "../../util/position";
import { chainSpriteCreation, SeqAnimation, BaseAnimation, Animation, runAsTween, ParAnimation, Create, runAnim } from "../../phaser/animation";
import { GameRefs } from "../../states/game";
import { changeAct, changeLevel, addNewSolution } from "./events";
import { addText, DataSprite, clearShader, addShader } from "../../phaser/datasprite";
import { ScreenAct, transitionScreen, ScreenExec } from "../transition";
import { selectedActId } from "../../data/saveData";
import { actData, ActDataKeys } from "../../data/actData";
import { LevelDataKeys } from "../../data/levelData";
import { TextPool } from "../../phaser/textpool";

export class ActScreen {
  actBtnPool: Pool<ActBtnData, "neutral" | "hover" | "down">
  levelBtnPool: Pool<LevelBtnData, "neutral" | "hover" | "down">
  solBtnPool: Pool<SolBtnData, "neutral" | "hover" | "down">
  bgSpritePool: Pool<BgSpriteData, {}>
  hoverSpritePool: Pool<HoverSpriteData, {}>
  hoverTextPool: TextPool
  messageIconPool: Pool<MessageIconData, {}>
  messagePool: Pool<MessageData, {}>
  
  openMessage: number | undefined = undefined

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.bgSpritePool = mkBgSpritePool(gameRefs);
    this.actBtnPool = mkActBtnPool(gameRefs);
    this.levelBtnPool = mkLevelBtnPool(gameRefs);
    this.solBtnPool = mkSolBtnPool(gameRefs);
    this.hoverSpritePool = mkHoverSpritePool(gameRefs);
    this.hoverTextPool = new TextPool(gameRefs.game);
    this.messageIconPool = mkMessageIconPool(gameRefs);
    this.messagePool = mkMessagePool(gameRefs);
  }

  draw() {
    const actId = this.gameRefs.saveData.currentActId;
    changeAct(this.gameRefs, actId);
  }

  drawBg(animations: boolean) {
    this.bgSpritePool.clear();

    const actId = this.gameRefs.saveData.currentActId;
    const bgSprite = actData[actId].bgSprite;
    
    const anim = new Create(() => {
      return this.bgSpritePool.newSprite(bgSprite.x, bgSprite.y, {}, { sprite: bgSprite.sprite, });
    }, self => {
      return new BaseAnimation(300, self, tween => {
        tween.from({ alpha: 0 }, 300, Phaser.Easing.Linear.None, false, 50);
      });
    });

    runAnim(true, this.gameRefs, anim);
  }

  drawActBtn(animations: boolean) {
    const anim = this.redrawActBtn();
    runAnim(animations, this.gameRefs, anim);
  }

  redrawActBtn(): Animation {
    this.actBtnPool.clear();

    const selActId = selectedActId(this.gameRefs);

    const anims: Animation[] = [];

    let i = 0;
    for (const actKey in actData) {
      const actId = Number(actKey) as ActDataKeys;

      let pos: Position;
      let frame: "down" | "neutral" | "hover";
      if (selActId !== undefined && selActId === actId) {
        // this is the currently selected act
        pos = createPosition(this.gameRefs.settings,
          "left", 100 + 150 * i, 200,
          "top", 0, 400,
        );
        frame = "down";
      } else {
        // this is not the currently selected act
        pos = createPosition(this.gameRefs.settings,
          "left", 100 + 150 * i, 400,
          "top", 0, 200,
        );
        frame = "neutral";
      }
      const sprite = actData[actId].icon;
      const anim: Animation = new Create(() => {
        return this.actBtnPool.newSprite(pos.xMin, pos.yMin, frame, { actId, actIndex: i, sprite });
      }, self => {
        return new BaseAnimation(150, self, tween => {
          tween.from({ y: -30, alpha: 0.5 }, 35, Phaser.Easing.Linear.None, false, 0);
        });
      });
      anims.push(anim);
      i += 1;
    }

    const parAnim = new SeqAnimation(anims);
    return parAnim;
  }

  drawLevelBtn(
    actId: ActDataKeys,
  ) {
    this.levelBtnPool.clear();
    this._drawLevelBtn(actId);
  }

  private _drawLevelBtn(
    actId: ActDataKeys,
  ) {
    const levels = actData[actId].levels;
    const anims = new ParAnimation(levels.map((levelData, levelIndex) => {
      const levelId = levelData.id;
      const pos = createPosition(this.gameRefs.settings,
        "left", levelData.iconLocation.x, 400,
        "top", levelData.iconLocation.y, 200,
      );
      if (levelData.dev !== undefined && levelData.dev && ! this.gameRefs.settings.devMode) {
        return new BaseAnimation(1, self, tween => {
          
        });
      } else {
        return new Create(() => {
          return this.levelBtnPool.newSprite(pos.xMin, pos.yMin, "neutral",
            { levelId, levelIndex, icon: levelData.icon }
          );
        }, self => {
          return new BaseAnimation(150, self, tween => {
            tween.from({ alpha: 0 }, 150, Phaser.Easing.Linear.None, false, 50);
          });
        });
      }
    }));

    runAsTween(this.gameRefs, anims);
  }

  drawMessageIcons(
    actId: ActDataKeys,
  ) {
    this.messageIconPool.clear();

    const messages = actData[actId].messages;
    console.log("test");

    messages.forEach((message, i) => {
      this.messageIconPool.newSprite(50 + 50 * i, 900, {},
        { 
          messageId: i,
        },
      );
    });
  }

  drawMessage(
    messageId: number,
  ) {
    const actId = this.gameRefs.saveData.currentActId;
    const message = actData[actId].messages[messageId];
    if (message !== undefined) {
      const anim = new Create(() => {
        const sprite = this.messagePool.newSprite(300, 300, {}, {});
        addText(this.gameRefs, sprite, { xMin: 10, yMin: 10, xMax: 100, yMax: 30 }, message, "#000000", 18);
        this.openMessage = messageId;
        return sprite;
      }, self => {
        return new BaseAnimation(150, self, tween => {
          tween.from({ alpha: 0 }, 150, Phaser.Easing.Linear.None, false, 50);
        });
      });

      this.messagePool.clear();
      runAsTween(this.gameRefs, anim);
    } else {
      throw `drawMessage, unknown message id ${messageId} for act id ${actId}`;
    }
  }

  clearMessage(
  ) {
    this.messagePool.clear();
  }

  setVisibility(
    visibility: boolean
  ) {
    this.actBtnPool.visible = visibility;
    this.levelBtnPool.visible = visibility;
    this.solBtnPool.visible = visibility;
    this.bgSpritePool.visible = visibility;
  }

  actSelectMode() {
    this.hoverSpritePool.clear();
    this.actBtnPool.visible = true;
    this.levelBtnPool.visible = true;
    this.solBtnPool.visible = false;
  }

  levelSelectMode() {
    this.actBtnPool.visible = true;
    this.levelBtnPool.visible = false;
    this.solBtnPool.visible = true;
  }

  clearAnimations() {
    this.gameRefs.game.tweens.removeFrom(this.actBtnPool, true);
    this.gameRefs.game.tweens.removeFrom(this.levelBtnPool, true);
    this.gameRefs.game.tweens.removeFrom(this.solBtnPool, true);
  }
}

type ActBtnData = {
  actId: ActDataKeys,
  actIndex: number,
  sprite: string,
}

function mkActBtnPool(
  gameRefs: GameRefs,
): Pool<ActBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return self.data.sprite;
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: -400 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.actIndex);
        }
      ],
      callbacks: {
        click: (self) => {
          transitionScreen(gameRefs, new ScreenAct(self.data.actId));
        },
        hoverOver: (self) => {
          const hoverBg = gameRefs.screens.actScreen.hoverSpritePool.newSprite(self.x + 200, self.y, {}, {});
          addText(gameRefs, hoverBg, { xMin: 10, xMax: 100, yMin: 10, yMax: 100 }, `Act ${self.data.actId + 1}`, "#000000", 20);
        },
        hoverOut: (self) => {
          gameRefs.screens.actScreen.hoverSpritePool.clear();
          gameRefs.screens.actScreen.hoverTextPool.clear();
        }
      },
    },
    self => { return self.data.actId === selectedActId(gameRefs); },
    status => {
      switch (status) {
        case "down": return "red-glow";
        case "hover": return "blue-glow";
        case "neutral": return undefined;
      }
    },
  );
}

type LevelBtnData = {
  levelId: LevelDataKeys,
  levelIndex: number,
  icon: string,
}

function mkLevelBtnPool(
  gameRefs: GameRefs,
): Pool<LevelBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return self.data.icon;
          case "hover": return self.data.icon;
          case "neutral": return self.data.icon;
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 50, Phaser.Easing.Linear.None, false, 0);
        }
      ],
      callbacks: {
        click: (self) => {
          transitionScreen(gameRefs, new ScreenExec(self.data.levelId));
        },
      },
    },
    self => { return false; },
    status => {
      switch (status) {
        case "down": return "red-glow";
        case "hover": return "blue-glow";
        case "neutral": return undefined;
      }
    },
  );
}

type SolBtnDataSelect = {
  tag: "SolBtnDataSelect",
  levelId: LevelDataKeys,
  solIndex: number,
}

type SolBtnDataAdd = {
  tag: "SolBtnDataAdd",
  levelId: LevelDataKeys,
}

type SolBtnData = SolBtnDataSelect | SolBtnDataAdd;

function mkSolBtnPool(
  gameRefs: GameRefs,
): Pool<SolBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs,
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
          const data = self.data;
          switch (data.tag) {
            case "SolBtnDataAdd": {
              addNewSolution(gameRefs, data.levelId);
              break;
            }
            case "SolBtnDataSelect": {
              // loadLevel(gameRefs, data.levelId, data.solIndex);
              break;
            }
          }
        },
      },
    },
    self => { return false; },
    status => {
      switch (status) {
        case "down": return "red-glow";
        case "hover": return "blue-glow";
        case "neutral": return undefined;
      }
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
      },
    },
  );
}

type HoverSpriteData = {
}

function mkHoverSpritePool(
  gameRefs: GameRefs,
): Pool<HoverSpriteData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "unit_select_bg.png";
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}

type MessageIconData = {
  messageId: number,
}

function mkMessageIconPool(
  gameRefs: GameRefs,
): Pool<MessageIconData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "expl_ch.png";
      },
      introAnim: [
      ],
      callbacks: {
        click: (self) => {
          if (gameRefs.screens.actScreen.openMessage === self.data.messageId) {
            gameRefs.screens.actScreen.clearMessage();
          } else {
            gameRefs.screens.actScreen.drawMessage(self.data.messageId);
          }
        },
      },
    },
  );
}

type MessageData = {
}

function mkMessagePool(
  gameRefs: GameRefs,
): Pool<MessageData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "unit_select_bg.png";
      },
      introAnim: [
      ],
      callbacks: {
      },
    },
  );
}