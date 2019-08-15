import { LevelData } from "./levelData";
import * as L from "./levelData";
import { isTrue, Equal } from "../../shared/type-util";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
  icon: string,
  bgSprite: { sprite: string, x: number, y: number },
}

// check that values of levelData are all `LevelData`
type ActDataValues = (typeof actData)[keyof (typeof actData)];
isTrue<Equal<ActDataValues, ActData>>(true);
export type ActDataKeys = keyof (typeof actData);

export const actData = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [L.a1l1, L.a1l2, L.a1l3, L.a1l5, L.a1l6, L.a1l8, L.a1l9, L.a1l10],
    icon: "icon_1_150_150.png",
    bgSprite: { sprite: "bg_a1.png", x: 100, y: 150 },
  },
  1: {
    shortName: "2",
    longName: "Act 2",
    levels: [L.a3l2, L.a3l3, L.a3l4, L.a3l2_1, L.a3l2_2, L.a3l3_1, L.a3l3_2, L.a3l4_1, L.a3l4_2],
    icon: "icon_2_150_150.png",
    bgSprite: { sprite: "select3_f.png", x: 550, y: 0 },
  },
  2: {
    shortName: "3",
    longName: "Act 3",
    levels: [L.a2l1],
    icon: "icon_3_150_150.png",
    bgSprite: { sprite: "hand_1000_1000.png", x: 400, y: 200 },
  },
}