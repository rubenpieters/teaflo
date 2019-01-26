import { Pool, mkButtonPool } from "../../phaser/pool";
import { GameRefs } from "../../states/game";
import { createPosition } from "../../util/position";
import { addText, DataSprite } from "../../phaser/datasprite";
import { chainSpriteCreation } from "../../phaser/animation";
import { levelAvailableCards } from "../act/data";
import { cardMap } from "../../data/cardMap";

export class LevelScreen {
  boxPool: Pool<{}, {}>
  buildCardPool: Pool<BuildCardData, {}>

  constructor(
    public readonly gameRefs: GameRefs
  ) {
    this.boxPool = mkBoxPool(gameRefs);
    this.buildCardPool = mkBuildCardPool(gameRefs);
  }

  drawBox(
  ) {
    this.boxPool.killAll();
    this.gameRefs.screens.bgScreen.bgOnIntroComplete(
      () => {
        this.createBox(true);
      },
    );
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

    const cards = levelAvailableCards(this.gameRefs);
    console.log(`CARDS: ${cards}`);
    const cardF = cards === undefined ? [] : cards.map((cardId, cardIndex) => {
      return {
        create: () => {
          const pos = createPosition(
            "left", 40 + cardIndex * 200, 150,
            "top", 40, 300,
          );
          const sprite = this.buildCardPool.newSprite(pos.xMin, pos.yMin, {}, { cardId });
          return sprite;
        },
        introTween: (sprite: DataSprite<BuildCardData>) => {
          return this.buildCardPool.introTween(sprite);
        },
      };
    });

    spriteFs = [boxF];
    spriteFs = spriteFs.concat(cardF);  
    chainSpriteCreation(spriteFs, animation);
  }

  setVisibility(
    visibility: boolean
  ) {
    this.boxPool.visible = visibility;
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
  )
}

type BuildCardData = {
  cardId: string,
}

function mkBuildCardPool(
  gameRefs: GameRefs,
): Pool<BuildCardData, {}> {
  return new Pool(
    gameRefs.game,
    {
      atlas: "atlas1",
      toFrame: (self, frameType) => { return cardMap[self.data.cardId] },
      introAnim: [
        (self, tween) => {
          tween.from({ y: self.y - 50 }, 60, Phaser.Easing.Linear.None, false, 30);
        },
      ],
      callbacks: {},
    },
  )
}
