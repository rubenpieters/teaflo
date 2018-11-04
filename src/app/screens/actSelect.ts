import { config } from "src/app/config";
import { createPoolButton } from "src/app/util/poolButton";
import { createButton } from "src/app/util/button";
import { createPosition } from "src/app/util/position";

// act -> button string mapping
export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
  2: "...",
};

export function actSelect_Main(
  game: Phaser.Game,
  actSelectBtnPool: Phaser.Group,
  levelSelectBtnPool: Phaser.Group,
  levelSelectGroup: Phaser.Group,
) {
  actSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );

    const button = createPoolButton(game, actSelectBtnPool, pos, btnString, "btn_act",
      () => levelSelect_Main(game, levelSelectBtnPool, levelSelectGroup, Number(actNumber))
    );
    if (i === 0) {
      first = button;
    }

    i += 1;
  }

  if (first !== undefined) {
    first.events.onInputUp.dispatch({ force: true });
  }
}

// act -> level id mapping
export const levelMap: { [key: number]: string[] | "info" } = {
  0: ["a1_l1"],
  1: ["a2_l1", "a2_l2", "a2_l3"],
  2: "info",
}

export function levelSelect_Main(
  game: Phaser.Game,
  levelSelectBtnPool: Phaser.Group,
  levelSelectGroup: Phaser.Group,
  act: number,
) {
  levelSelectBtnPool.forEachAlive((x: Phaser.Sprite) => x.kill());
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const levelId of levelMap[act]) {
    if (levelMap[act] === "info") {

    } else {
      let btnString = levelId;
  
      const pos = createPosition(
        "left", 250, config.levelButtonWidth,
        "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
      );
  
      const button = createPoolButton(game, levelSelectBtnPool, pos, btnString, "btn_level",
        () => levelSelect_Info(game, levelSelectGroup)
      );
      if (i === 0) {
        first = button;
      }
  
      i += 1;
    }
  }

  if (first !== undefined) {
    first.events.onInputUp.dispatch({ force: true });
  }
}

export function levelSelect_Select(
  game: Phaser.Game,
) {

}

export function levelSelect_Info(
  game: Phaser.Game,
  levelSelectGroup: Phaser.Group,
) {
  levelSelectGroup.forEachAlive((x: Phaser.Sprite) => x.destroy());

  const bgSpritePos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );
  console.log(`BG: ${JSON.stringify(bgSpritePos)}`);
  const bgSprite = game.add.sprite(bgSpritePos.xMin, bgSpritePos.yMin, "bg_level", undefined, levelSelectGroup)

  const startBtnPos = createPosition(
    "right", 500, config.levelButtonWidth,
    "top", 1200, config.levelButtonHeight,
  );
  const startBtn = createButton(game, levelSelectGroup, startBtnPos, "Start", "btn_level",
    () => console.log("click")
  );
}