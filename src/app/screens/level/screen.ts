import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { chainSpriteCreation } from "../../phaser/animation";
import { currentSchemSol } from "../act/data";
import { cardMap } from "../../data/cardMap";
import { moveCard, moveCardToFirstFree } from "../level/events";
import { filterUndefined } from "../../util/util";

export class LevelScreen {
  boxPool: Pool<{}, {}>
  buildCardPool: Pool<BuildCardData | BuildSlotData, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.boxPool = mkBoxPool(gameRefs);
    this.buildCardPool = mkBuildCardPool(gameRefs);
  }

  drawBox(
  ) {
    this.boxPool.killAll();
    this.buildCardPool.killAll();
    this.gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => {
        this.createBox(true);
      },
    );
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
        const sprite = this.boxPool.newSprite(pos.xMin, pos.yMin, {}, {});
        return sprite;
      },
      introTween: (sprite: DataSprite<{}>) => {
        return this.boxPool.introTween(sprite);
      },
    };

    const solData = currentSchemSol(this.gameRefs);
    const supplyF = solData === undefined ? [] : solData.supply.map((cardId, cardIndex) => {
      const data: BuildCardData | BuildSlotData = cardId === undefined ?
        { tag: "slot", index: cardIndex } : { tag: "card", cardId, type: "supply", index: cardIndex };
      return {
        create: () => {
          const pos = createPosition(
            "left", 40 + cardIndex * 200, 150,
            "top", 40, 300,
          );
          const sprite = this.buildCardPool.newSprite(pos.xMin, pos.yMin, {}, data);
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
  }

  setVisibility(
    visibility: boolean
  ) {
    this.boxPool.visible = visibility;
    this.buildCardPool.visible = visibility;
  }
}

function mkBoxPool(
  gameRefs: GameRefs,
): Pool<{}, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "box",
      toFrame: (self, frameType) => { return <any>undefined },
      introAnim: [
        (self, tween) => {
          tween.from({ x: self.x - 640 }, 75, Phaser.Easing.Linear.None, false, 50);
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
          tween.from({ y: self.y - 50 }, 60, Phaser.Easing.Linear.None, false, 20);
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