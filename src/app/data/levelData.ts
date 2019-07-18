import { isTrue, Equal } from "../../shared/type-util";
import { FrUnitId } from "../../shared/data/frUnitMap";
import { EnUnitId } from "../../shared/data/enUnitMap";

export type LevelData = {
  name: string,
  icon: string,
  iconLocation: { x: number, y: number },
  id: LevelDataKeys,
  cardIds: FrUnitId[][],
  enemyIds: EnUnitId[],
  slots: number,
  dev?: boolean,
}

export const a1l1: LevelData = {
  name: "A1 level1",
  id: "a1l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["a1l1_fr"]],
  enemyIds: ["a1l1_en"],
  slots: 1,
};

export const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 250 },
  cardIds: [["a1l2_fr"], ["c_i"]],
  enemyIds: ["a1l2_en"],
  slots: 2,
};

export const a1l3: LevelData = {
  name: "A1 level3",
  id: "a1l3",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["d_i"], ["c_iii"]],
  enemyIds: ["a1l3_en1"],
  slots: 2,
};

export const a1l4: LevelData = {
  name: "A1 level4",
  id: "a1l4",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 550, y: 250 },
  cardIds: [["a1l4_fr1"], ["a1l4_fr2"]],
  enemyIds: ["a1l4_en1", "a1l4_en2"],
  slots: 2,
};

export const a1l5: LevelData = {
  name: "A1 level5",
  id: "a1l5",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 400 },
  cardIds: [["a1l5_fr3"], ["a1l5_fr2"], ["a1l5_fr1"]],
  enemyIds: ["a1l5_en1", "a1l5_en2"],
  slots: 3,
};

export const a1l6: LevelData = {
  name: "A1 level6",
  id: "a1l6",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 400 },
  cardIds: [["a1l6_fr3"], ["a1l6_fr2"], ["a1l6_fr1"]],
  enemyIds: ["a1l6_en1", "a1l6_en2"],
  slots: 3,
};

export const a1l7: LevelData = {
  name: "A1 level7",
  id: "a1l7",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 400 },
  cardIds: [["a1l7_fr1"], ["a1l7_fr2"], ["a1l7_fr3"]],
  enemyIds: ["a1l7_en1"],
  slots: 3,
  dev: true,
};


export const a2l1: LevelData = {
  name: "A2 level1",
  id: "a2l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["trinity_dmg"], ["trinity_sup"], ["trinity_tnk"]],
  enemyIds: ["a2l1_en"],
  slots: 3,
};

export const a3l1: LevelData = {
  name: "A3 level1",
  id: "a3l1",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l1_en1"],
  slots: 3,
};

export const a3l2: LevelData = {
  name: "A3 level2",
  id: "a3l2",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 250, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l2_en1"],
  slots: 3,
};

export const a3l3: LevelData = {
  name: "A3 level3",
  id: "a3l3",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l3_en1", "a3l3_en2"],
  slots: 3,
};

export const a3l4: LevelData = {
  name: "A3 level4",
  id: "a3l4",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 550, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l4_en1"],
  slots: 3,
};

export const a3l5: LevelData = {
  name: "A3 level5",
  id: "a3l5",
  icon: "lvl_icon_150_150.png",
  iconLocation: { x: 700, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l5_en1"],
  slots: 3,
};

// check that values of levelData are all `LevelData`
type LevelDataValues = (typeof levelData)[keyof (typeof levelData)];
isTrue<Equal<LevelDataValues, LevelData>>(true);
export type LevelDataKeys = keyof (typeof levelData);

export const levelData = {
  "a1l1": a1l1,
  "a1l2": a1l2,
  "a1l3": a1l3,
  "a1l4": a1l4,
  "a1l5": a1l5,
  "a1l6": a1l6,
  "a1l7": a1l7,
  "a2l1": a2l1,
  "a3l1": a3l1,
  "a3l2": a3l2,
  "a3l3": a3l3,
  "a3l4": a3l4,
  "a3l5": a3l5,
}
