import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { chainSpriteCreation, createTween } from "../../phaser/animation";
import { currentSchemSol, selectedSchem, levelData } from "../act/data";
import { cardMap } from "../../data/cardMap";
import { moveCard, moveCardToFirstFree, loadLevel, newExecLevel, levelStats } from "../level/events";
import { filterUndefined } from "../../util/util";

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
  buildCardPool: Pool<BuildCardData | BuildSlotData, {}>
  execStartBtnPool: Pool<{}, "neutral" | "hover" | "down">

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.boxPool = mkBoxPool(gameRefs);
    this.buildCardPool = mkBuildCardPool(gameRefs);
    this.execStartBtnPool = mkExecStartBtnPool(gameRefs);
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
    this.boxPool.killAll();
    this.buildCardPool.killAll();
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

    const supplyF = solData === undefined ? [] : solData.supply.map((cardId, cardIndex) => {
      const data: BuildCardData | BuildSlotData = cardId === undefined ?
        { tag: "slot", index: cardIndex } : { tag: "card", cardId, type: "supply", index: cardIndex };
      return {
        create: () => {
          const loc = levelData[schem!.levelId].supplyLocations[cardIndex];
          const sprite = this.buildCardPool.newSprite(loc.x, loc.y, {}, data);
          if (animation) {
            sprite.alpha = 0;
          }
          return sprite;
        },
        introTween: (sprite: DataSprite<BuildCardData | BuildSlotData>) => {
          return this.buildCardPool.introTween(sprite);
        },
      };
    });
    const deployF = solData === undefined ? [] : solData.deploy.map((cardId, cardIndex) => {
      const data: BuildCardData | BuildSlotData = cardId === undefined ?
        { tag: "slot", index: cardIndex } : { tag: "card", cardId, type: "deploy", index: cardIndex };
      return {
        create: () => {
          const pos = createPosition(
            "left", 840 + cardIndex * 200, 150,
            "top", 80, 300,
          );
          const sprite = this.buildCardPool.newSprite(pos.xMin, pos.yMin, {}, data);
          if (animation) {
            sprite.alpha = 0;
          }
          return sprite;
        },
        introTween: (sprite: DataSprite<BuildCardData | BuildSlotData>) => {
          return this.buildCardPool.introTween(sprite);
        },
      };
    });

    spriteFs = [boxF];
    spriteFs = spriteFs.concat(supplyF).concat(deployF);
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
      "right", 400, 400,
      "bot", 300, 200,
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
  type: "supply" | "deploy",
  index: number,
}

type BuildSlotData = {
  tag: "slot",
  index: number,
}

function mkBuildCardPool(
  gameRefs: GameRefs,
): Pool<BuildCardData | BuildSlotData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => {
        switch (self.data.tag) {
          case "card": return cardMap[self.data.cardId];
          case "slot": return "en_unit_a1_l1_01.png";
        }
      },
      introAnim: [
        (self, tween) => {
          tween.to({ alpha: 1 }, 20, Phaser.Easing.Linear.None, false, 5);
        },
      ],
      callbacks: {
        click: (self) => {
          switch (self.data.tag) {
            case "card": {
              const opposite = self.data.type === "supply" ? "deploy" : "supply";
              moveCardToFirstFree(gameRefs, {
                type: self.data.type, index: self.data.index,
              }, {
                type: opposite
              });
              break;
            }
            case "slot": {
              return;
            }
          }
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