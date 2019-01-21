import { actData, selectedActId } from "../../screens/act/data";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { settings } from "../../data/settings";
import { createPosition } from "../../util/position";
import { createTween } from "../../phaser/animation";
import { GameRefs } from "../../states/game";
import { changeAct } from "./events";
import { addText } from "../../phaser/datasprite";

export class ActScreen {
  actBtnPool: Pool<ActBtnData, "neutral" | "hover" | "down">
  levelBtnPool: Pool<LevelBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.actBtnPool = mkActBtnPool(gameRefs);
    this.levelBtnPool = mkLevelBtnPool(gameRefs);
  }

  drawActBtn() {
    this.redrawActBtn();
    this.actBtnPool.playIntroAnimations();
  }

  redrawActBtn() {
    this.actBtnPool.killAll();

    let i = 0;
    for (const actKey in actData) {
      const actId = Number(actKey);
      
      const selActId = selectedActId(this.gameRefs);
      if (selActId !== undefined && selActId === actId) {
        // this is the currently selected act
        const pos = createPosition(
          "left", 100 + 210 * i, 400,
          "top", -100, 200,
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
    this.gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => {
        this.levelBtnPool.killAll();
      },
      () => {
        this._drawLevelBtn(actId);
      },
    );
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

  setVisibility(
    visibility: boolean
  ) {
    this.actBtnPool.visible = visibility;
    this.levelBtnPool.visible = visibility;
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
      spritesheet: "btn",
      toFrame: frameType => {
        switch (frameType) {
          case "down": return 1;
          case "hover": return 2;
          case "neutral": return 0;
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: -400 }, 150, Phaser.Easing.Linear.None, false, 30 * self.data.actIndex);
        }
      ],
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
      spritesheet: "btn_level",
      toFrame: frameType => {
        switch (frameType) {
          case "down": return 1;
          case "hover": return 2;
          case "neutral": return 0;
        }
      },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 50, Phaser.Easing.Linear.None, false, 0);
        }
      ],
      callbacks: {
        click: (self) => {
          console.log(`click: ${self.data.levelId}`);
        },
      },
    },
    self => { return false; }
  );
}