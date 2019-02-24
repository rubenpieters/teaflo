import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { chainSpriteCreation, createTween } from "../../phaser/animation";
import { currentSchemSol, selectedSchem, levelData } from "../act/data";
import { cardMap } from "../../data/cardMap";
import { loadLevel, newExecLevel, levelStats, toggleDeploy } from "../level/events";
import { filterUndefined } from "../../util/util";
import { settings } from "../../data/settings";

const colors =
  [
    0x469990,
    0xfabebe,
    0x42d4f4,
    0x800000,
    0x800000,
  ]

export class LevelScreen {
  boxPool: Pool<BoxData, {}>
  box: Phaser.Sprite | undefined
  buildCardPool: Pool<BuildCardData, {}>
  execStartBtnPool: Pool<{}, "neutral" | "hover" | "down">
  circPool: Pool<{}, {}>
  lineGraphicsPool: Phaser.Graphics

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.boxPool = mkBoxPool(gameRefs);
    this.buildCardPool = mkBuildCardPool(gameRefs);
    this.execStartBtnPool = mkExecStartBtnPool(gameRefs);
    this.circPool = mkCircPool(gameRefs);
    this.lineGraphicsPool = gameRefs.game.add.graphics();
  }

  drawBox(
  ) {
    this.boxPool.killAll();
    this.buildCardPool.killAll();
    this.execStartBtnPool.killAll();
    
    this.createBox(true);
    this.drawExecStartBtn();
  }

  redrawBox(
  ) {
    this.boxPool.clear();
    this.buildCardPool.clear();
    this.lineGraphicsPool.clear();
    this.circPool.clear();
    this.createBox(false);
  }

  createBox(
    animation: boolean,
  ) {
    const schem = selectedSchem(this.gameRefs);
    const levelId = schem!.levelId;
    const solData = currentSchemSol(this.gameRefs);

    let spriteFs: {
      create: () => DataSprite<any>,
      introTween: (sprite: DataSprite<any>) => { first: Phaser.Tween, last: Phaser.Tween } | undefined,
    }[];
    const boxF = {
      create: () => {
        const pos = createPosition(
          "left", 0, 640,
          "top", 0, 1080,
        );
        const sprite = this.boxPool.newSprite(pos.xMin, pos.yMin, {}, { sprite: levelData[levelId].boxSprite });
        this.box = sprite;
        return sprite;
      },
      introTween: (sprite: DataSprite<BoxData>) => {
        return this.boxPool.introTween(sprite);
      },
    };

    const supplyF = solData === undefined ? [] : solData.supply.map(({ cardId, deployPos }, cardIndex) => {
      let data: BuildCardData = { tag: "card", cardId, deployPos, index: cardIndex };
      return {
        create: () => {
          const loc = levelData[schem!.levelId].supplyLocations[cardIndex];
          const sprite = this.buildCardPool.newSprite(loc.x, loc.y, {}, data);
          if (animation) {
            sprite.alpha = 0;
          }
          if (deployPos === undefined) {
            sprite.tint = 0xAAAAAA;
            sprite.alpha = 1;
          } else {
            sprite.tint = colors[deployPos];
            sprite.alpha = 0.5;

            // draw line
            this.lineGraphicsPool.beginFill();
            this.lineGraphicsPool.lineStyle(8); //, colors[deployPos], 1);
            this.lineGraphicsPool.moveTo(loc.x + 200, loc.y + 100);
            this.lineGraphicsPool.lineTo(settings.gameWidth - 550 + 60 * deployPos, 500);
            this.lineGraphicsPool.endFill();

            // draw circle
            const circ = this.circPool.newSprite(settings.gameWidth - 550 + 60 * deployPos - 15, 500 - 15, {}, {});
            circ.animations.add("anim", ["sel_circ_1.png", "sel_circ_2.png", "sel_circ_3.png", "sel_circ_4.png"], 8, true);
            circ.animations.play("anim");
          }
          return sprite;
        },
        introTween: (sprite: DataSprite<BuildCardData>) => {
          return this.buildCardPool.introTween(sprite);
        },
      };
    });

    spriteFs = [boxF];
    spriteFs = spriteFs.concat(supplyF); //.concat(deployF);
    chainSpriteCreation(spriteFs, animation);
    //chainSpriteCreation(deployF, animation);
  }

  drawExecStartBtn(
  ) {
    this.redrawExecStartBtn();
    this.execStartBtnPool.playIntroAnimations();
  }

  redrawExecStartBtn(
  ) {
    this.execStartBtnPool.killAll();

    const pos = createPosition(
      "right", 425, 400,
      "bot", 100, 200,
    );
    const sprite = this.execStartBtnPool.newSprite(pos.xMin, pos.yMin, "neutral", {});
    addText(this.gameRefs, sprite, pos, "Start", "#000000", 40);
  }

  setVisibility(
    visibility: boolean
  ) {
    this.boxPool.visible = visibility;
    this.buildCardPool.visible = visibility;
    this.execStartBtnPool.visible = visibility;
  }

  clearAnimations() {
    this.gameRefs.game.tweens.removeFrom(this.boxPool, true);
    this.gameRefs.game.tweens.removeFrom(this.buildCardPool, true);
    this.gameRefs.game.tweens.removeFrom(this.execStartBtnPool, true);
  }
}

type BoxData = {
  sprite: string,
}

function mkBoxPool(
  gameRefs: GameRefs,
): Pool<BoxData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => { return self.data.sprite; },
      introAnim: [
        (self, tween) => {
          tween.from({ alpha: 0 }, 125, Phaser.Easing.Linear.None, false, 0);
        },
      ],
      callbacks: {},
    },
  );
}

type BuildCardData = {
  tag: "card",
  cardId: string,
  deployPos: number | undefined,
  index: number,
}

function mkBuildCardPool(
  gameRefs: GameRefs,
): Pool<BuildCardData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return cardMap[self.data.cardId];
      },
      introAnim: [
        (self, tween) => {
          tween.to({ alpha: 1 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          toggleDeploy(gameRefs, self.data.index);
        },
      },
    },
  );
}

function mkExecStartBtnPool(
  gameRefs: GameRefs,
): Pool<{}, "neutral" | "hover" | "down"> {
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
          const schem = selectedSchem(gameRefs);
          if (schem !== undefined) {
            const box = gameRefs.screens.levelScreen.box;
            if (box !== undefined) {
              const boxToLeftTween = createTween(gameRefs.game, box,
                (tween) => {
                  tween.to({ x: - 640 }, 50, Phaser.Easing.Linear.None);
                },
              );

              boxToLeftTween.onComplete.add(() => {
                newExecLevel(gameRefs, schem.levelId, schem.solId);
              });
              boxToLeftTween.start();
            }
          }
        },
      },
    },
    self => { return false; }
  );
}


function mkCircPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        return "sel_circ_1.png";
      },
      introAnim: [
        (self, tween) => {
          tween.to({ alpha: 1 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
      },
    },
  );
}