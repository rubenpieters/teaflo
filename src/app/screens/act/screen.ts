import { actData, selectedActId, selectedMenu, SelectedActMenu, SelectedLevelMenu, LevelData } from "../../screens/act/data";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { settings } from "../../data/settings";
import { createPosition } from "../../util/position";
import { createTween, chainSpriteCreation } from "../../phaser/animation";
import { GameRefs } from "../../states/game";
import { changeAct, changeLevel, addNewSolution } from "./events";
import { addText, DataSprite } from "../../phaser/datasprite";
import { loadLevel, levelStats } from "../level/events";
import { ScreenAct, transitionScreen } from "../transition";

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
    const menu = selectedMenu(this.gameRefs);
    if (menu === undefined) {
      this.drawActBtn();
      return;
    }
    switch (menu.tag) {                                                                                             
      case "SelectedActMenu": {
        changeAct(this.gameRefs, menu.actId);
        break;
      }
      case "SelectedLevelMenu": {
        changeLevel(this.gameRefs, menu.levelId);
        break;
      }
    }
  }

  drawActBtn() {
    this.redrawActBtn();
    this.actBtnPool.playIntroAnimations();
  }

  redrawActBtn() {
    this.actBtnPool.killAll();

    const selActId = selectedActId(this.gameRefs);
    let i = 0;
    for (const actKey in actData) {
      const actId = Number(actKey);
      
      if (selActId !== undefined && selActId === actId) {
        // this is the currently selected act
        const pos = createPosition(
          "left", 100 + 210 * i, 200,
          "top", -100, 400,
        );
        this.actBtnPool.newSprite(pos.xMin, pos.yMin, "down", { actId, actIndex: i, });
      } else {
        const pos = createPosition(
          "left", 100 + 210 * i, 400,
          "top", -200, 200,
        );
        // this is not the currently selected act
        this.actBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { actId, actIndex: i, });
      }
      i += 1;
    }
  }

  drawLevelBtn(
    actId: number,
  ) {
    this.levelBtnPool.clear();
    this._drawLevelBtn(actId);
    /*this.gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => {
        this._drawLevelBtn(actId);
        const levels = actData[actId].levels;
        this.levelSelectionPool.newSprite(0, 0, {}, {
          polys: levels.map(x => new Phaser.Polygon(x.spritePoints.map(({x, y}) => new Phaser.Point(x, y)))),
          levelData: levels,
          bgSprite: actData[actId].bgSprite,
        });
      },
    );*/
  }

  private _drawLevelBtn(
    actId: number,
  ) {
    this.createLevelBtn(actId, 0);
  }

  private createLevelBtn(
    actId: number,
    levelIndex: number,
  ) {
    const levelData = actData[actId].levels[levelIndex];
    const levelId = levelData.id;
    const pos = createPosition(
      "left", 250, 400,
      "top", 300 + (250 * levelIndex), 200,
    );
    const sprite = this.levelBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { levelId, levelIndex });
    addText(this.gameRefs, sprite, pos, levelData.name, "#000000", 40);
    const intro = this.levelBtnPool.introTween(sprite);
    if (intro !== undefined) {
      if (levelIndex + 1 < actData[actId].levels.length) {
        intro.last.onComplete.add(() => {
          this.createLevelBtn(actId, levelIndex + 1);
        });
      }
      intro.first.start();
    }
  }

  redrawSolBtn(
    levelId: string,
  ) {
    this.solBtnPool.killAll();
    this._drawSolBtn(levelId, false);
  }

  drawSolBtn(
    levelId: string,
  ) {
    this.solBtnPool.killAll();
    this._drawSolBtn(levelId, true);
  }

  private _drawSolBtn(
    levelId: string,
    animation: boolean,
  ) {
    const levels = this.gameRefs.saveData.act.levels[levelId];
    let solBtns: {
      create: () => DataSprite<SolBtnData>,
      introTween: (sprite: DataSprite<SolBtnData>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[];
    let solAddI = 0;
    if (levels === undefined) {
      solBtns = [];
    } else {
      solBtns = levels.map((levelData, solIndex) => {
        return {
          create: () => {
            const pos = createPosition(
              "left", 250, 400,
              "top", 300 + (250 * solIndex), 200,
            );
            const sprite = this.solBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { tag: "SolBtnDataSelect", levelId, solIndex });
            const lvlStats = levelStats(this.gameRefs, levelId, solIndex);
            addText(this.gameRefs, sprite, pos, `${levelData.name}\nWin: ${lvlStats.win}`, "#000000", 40);
            return sprite;
          },
          introTween: (sprite: DataSprite<SolBtnData>) => {
            return this.solBtnPool.introTween(sprite);
          },
        }
      });
      solAddI = levels.length;
    }
    const solAddBtn = {
      create: () => {
        const pos = createPosition(
          "left", 250, 400,
          "top", 300 + (250 * solAddI), 200,
        );
        const sprite = this.solBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { tag: "SolBtnDataAdd", levelId });
        addText(this.gameRefs, sprite, pos, "+", "#000000", 40);
        return sprite;
      },
      introTween: (sprite: DataSprite<SolBtnData>) => {
        return this.solBtnPool.introTween(sprite);
      },
    };
    chainSpriteCreation(solBtns.concat(solAddBtn), animation);
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
  actId: number,
  actIndex: number,
}

function mkActBtnPool(
  gameRefs: GameRefs,
): Pool<ActBtnData, "neutral" | "hover" | "down"> {
  return mkButtonPool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (frameType) {
          case "down": return <any>"bmark_click.png";
          case "hover": return <any>"bmark_hover.png";
          case "neutral": return <any>"bmark_neutral.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: -400 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.actIndex);
        }
      ],
      callbacks: {
        click: (self) => {
          transitionScreen(gameRefs, new ScreenAct(new SelectedActMenu(self.data.actId)))
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
    self => { return self.data.actId === selectedActId(gameRefs); }
  );
}

type LevelBtnData = {
  levelId: string,
  levelIndex: number,
}

function mkLevelBtnPool(
  gameRefs: GameRefs,
): Pool<LevelBtnData, "neutral" | "hover" | "down"> {
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
          changeLevel(gameRefs, self.data.levelId);
        },
      },
    },
    self => { return false; }
  );
}

type SolBtnDataSelect = {
  tag: "SolBtnDataSelect",
  levelId: string,
  solIndex: number,
}

type SolBtnDataAdd = {
  tag: "SolBtnDataAdd",
  levelId: string,
}

type SolBtnData = SolBtnDataSelect | SolBtnDataAdd;

function mkSolBtnPool(
  gameRefs: GameRefs,
): Pool<SolBtnData, "neutral" | "hover" | "down"> {
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
          const data = self.data;
          switch (data.tag) {
            case "SolBtnDataAdd": {
              addNewSolution(gameRefs, data.levelId);
              break;
            }
            case "SolBtnDataSelect": {
              loadLevel(gameRefs, data.levelId, data.solIndex);
              break;
            }
          }
        },
      },
    },
    self => { return false; }
  );
}