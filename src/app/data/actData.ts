import { LevelData } from "./levelData";
import * as L from "./levelData";
import { isTrue, Equal } from "../../shared/type-util";

export type ActData = {
  shortName: string,
  longName: string,
  levels: LevelData[],
  bgSprite: string,
}

// check that values of levelData are all `LevelData`
type ActDataValues = (typeof actData)[keyof (typeof actData)];
isTrue<Equal<ActDataValues, ActData>>(true);
export type ActDataKeys = keyof (typeof actData);

export const actData = {
  0: {
    shortName: "1",
    longName: "Act 1",
    levels: [L.a1l1, L.a1l2, L.a1l3, L.a1l4, L.a1l5, L.a1l6, L.a1l7],
    bgSprite: "sel_act1.png",
  },
  1: {
    shortName: "2",
    longName: "Act 2",
    levels: [L.a3l2, L.a3l3, L.a3l4, L.a3l2_1, L.a3l2_2, L.a3l3_1, L.a3l3_2, L.a3l4_1, L.a3l4_2],
    bgSprite: "sel_act1.png",
  },
  2: {
    shortName: "3",
    longName: "Act 3",
    levels: [L.a2l1],
    bgSprite: "sel_act1.png",
  },
}