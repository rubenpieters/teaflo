import { Pool, mkButtonPool } from "../../phaser/pool";
import { createPosition, Position } from "../../util/position";
import { chainSpriteCreation, SeqAnimation, BaseAnimation, Animation, runAsTween, ParAnimation, Create } from "../../phaser/animation";
import { GameRefs } from "../../states/game";
import { changeAct, changeLevel, addNewSolution } from "./events";
import { addText, DataSprite, clearShader, addShader } from "../../phaser/datasprite";
import { ScreenAct, transitionScreen, ScreenExec } from "../transition";
import { selectedActId } from "../../data/saveData";
import { actData, ActDataKeys } from "../../data/actData";
import { LevelDataKeys } from "../../data/levelData";

export class ActScreen {
  actBtnPool: Pool<ActBtnData, "neutral" | "hover" | "down">
  levelBtnPool: Pool<LevelBtnData, "neutral" | "hover" | "down">
  solBtnPool: Pool<SolBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.actBtnPool = mkActBtnPool(gameRefs);
    this.levelBtnPool = mkLevelBtnPool(gameRefs);
    this.solBtnPool = mkSolBtnPool(gameRefs);
  }

  draw() {
    const actId = this.gameRefs.saveData.currentActId;
    changeAct(this.gameRefs, actId);
  }

  drawActBtn() {
    const anim = this.redrawActBtn();
    runAsTween(this.gameRefs, anim);
  }

  redrawActBtn(): Animation {
    this.actBtnPool.killAll();

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
          "left", 100 + 210 * i, 200,
          "top", 0, 400,
        );
        frame = "down";
      } else {
        // this is not the currently selected act
        pos = createPosition(this.gameRefs.settings,
          "left", 100 + 210 * i, 400,
          "top", 0, 200,
        );
        frame = "neutral";
      }
      const anim: Animation = new Create(() => {
        return this.actBtnPool.newSprite(pos.xMin, pos.yMin, frame, { actId, actIndex: i, });
      }, self => {
        return new BaseAnimation(150, self, tween => {
          tween.from({ y: -400 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.actIndex);
        });
      });
      anims.push(anim);
      i += 1;
    }

    const parAnim = new ParAnimation(anims);
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
            tween.from({ alpha: 0 }, 300, Phaser.Easing.Linear.None, false, 50);
          });
        });
      }
    }));

    runAsTween(this.gameRefs, anims);
  }

  setVisibility(
    visibility: boolean
  ) {
    this.actBtnPool.visible = visibility;
    this.levelBtnPool.visible = visibility;
    this.solBtnPool.visible = visibility;
  }

  actSelectMode() {
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
}

function mkActBtnPool(
  gameRefs: GameRefs,
): Pool<ActBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return "test2_200_200.png";
          case "hover": return "test2_200_200.png";
          case "neutral": return "test2_200_200.png";
        }
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