import { config } from "../config";
import { createButton, killButton } from "../util/button";
import { createPosition } from "../util/position";

// act -> button string mapping
export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
  2: "...",
};

export function actSelect_Main(
  game: Phaser.Game,
  actSelectPool: Phaser.Group,
  levelSelectPool: Phaser.Group,
) {
  actSelectPool.forEachAlive((x: Phaser.Sprite) => killButton(x));
  let allButtons: Phaser.Sprite[] = [];

  let i = 0;
  for (const actNumber in actNumberMap) {
    let btnString = actNumberMap[actNumber];

    const pos = createPosition(
      "left", 100 + config.actButtonWidth * i, config.actButtonWidth,
      "bot", 0, config.actButtonHeight,
    );

    const button = createButton(game, actSelectPool, pos, btnString, "btn_act", allButtons,
      () => levelSelect_Main(game, levelSelectPool, Number(actNumber))
    );

    allButtons.push(button);
    i += 1;
  }

  allButtons[0].events.onInputUp.dispatch();
}

// act -> level id mapping
export const levelMap: { [key: number]: string[] | "info" } = {
  0: ["a1_l1"],
  1: ["a2_l1", "a2_l2", "a2_l3"],
  2: "info",
}

export function levelSelect_Main(
  game: Phaser.Game,
  levelSelectPool: Phaser.Group,
  act: number,
) {
  levelSelectPool.forEachAlive((x: Phaser.Sprite) => killButton(x));
  let allButtons: Phaser.Sprite[] = [];

  let i = 0;
  if (levelMap[act] === "info") {

  } else {
    for (const levelId of levelMap[act]) {
      let btnString = levelId;
  
      const pos = createPosition(
        "left", 800, config.levelButtonWidth,
        "top", 400 + (config.levelButtonHeight + 50) * i, config.levelButtonHeight,
      );
  
      const button = createButton(game, levelSelectPool, pos, btnString, "btn_level", allButtons,
        undefined
      );
  
      allButtons.push(button);
      i += 1;
    }

    allButtons[0].events.onInputUp.dispatch();
  }
}