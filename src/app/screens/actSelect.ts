import { config } from "../config";
import { createPoolButton, killPoolButton } from "src/app/util/poolButton";
import { createButton, killButton } from "src/app/util/button";
import { createPosition } from "../util/position";

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
) {
  actSelectBtnPool.forEachAlive((x: Phaser.Sprite) => killPoolButton(x));
  let first: Phaser.Sprite | undefined = undefined;

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );

    const button = createPoolButton(game, actSelectBtnPool, pos, btnString, "btn_act",
      () => levelSelect_Main(game, levelSelectBtnPool, Number(actNumber))
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
  act: number,
) {
  levelSelectBtnPool.forEachAlive((x: Phaser.Sprite) => killPoolButton(x));
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
        undefined
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
) {
  const pos = createPosition(
    "right", 250, config.levelBgWidth,
    "top", 400, config.levelBgHeight,
  );

  /*const button = createPoolButton(game, levelSelectBtnPool, pos, btnString, "btn_level",
    undefined
  );*/
}