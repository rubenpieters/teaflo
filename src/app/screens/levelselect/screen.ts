import { GameRefs } from "../../states/game";
import { Pool, mkButtonPool } from "../../phaser/pool";
import { levelMap } from "./data";
import { createPosition } from "../../util/position";
import { addText } from "../../phaser/datasprite";

export class LevelSelectScreen {
  bgPool: Pool<BgData, {}>
  levelBtnPool: Pool<LevelBtnData, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.bgPool = mkBgPool(gameRefs);
    this.levelBtnPool = mkLevelBtnPool(gameRefs);
  }

  clear() {
    this.bgPool.killAll();
    this.levelBtnPool.killAll();
  }

  draw(
    actId: number,
  ) {
    this.redraw(actId);
    this.bgPool.playIntroAnimations();
  }

  redraw(
    actId: number,
  ) {
    this.clear();

    const sprite = this.bgPool.newSprite(0, 0, {}, { actId });
    sprite.loadTexture("bg0");
  }

  drawMenu(
    actId: number,
  ) {
    this.redrawMenu(actId);
    this.levelBtnPool.playIntroAnimations();
  }

  redrawMenu(
    actId: number,
  ) {
    this.levelBtnPool.killAll();

    let levelIndex = 0;
    for (const levelData of levelMap[actId]) {
      const levelId = levelData.id;
      const pos = createPosition(
        "left", 250, 400,
        "top", 300 + (250 * levelIndex), 200,
      );
      const sprite = this.levelBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", { levelId, levelIndex });
      addText(this.gameRefs, sprite, pos, levelData.name, "#000000", 40);

      levelIndex += 1;
    }
  }

  setVisibility(
    visibility: boolean
  ) {
    this.bgPool.visible = visibility;
  }
}

type BgData = {
  actId: number,
}

function mkBgPool(
  gameRefs: GameRefs,
): Pool<BgData, {}> {
  return new Pool(
    gameRefs.game,
    {
      spritesheet: "bg0",
      toFrame: frameType => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg1"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg2"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg3"));
        },
        (self, tween) => {
          tween.from({}, 30);
          tween.onComplete.add(() => self.loadTexture("bg4"));
          gameRefs.screens.levelSelectScreen.drawMenu(self.data.actId);
        },
      ],
      callbacks: {},
    },
  )
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
          tween.from({ y: self.y - 100 }, 100, Phaser.Easing.Linear.None, false, 30 * self.data.levelIndex);
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