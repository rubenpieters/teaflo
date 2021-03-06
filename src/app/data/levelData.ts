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
  icon: "icon_a1l1_327_142.png",
  iconLocation: { x: 890, y: 165 },
  cardIds: [["a1l1_fr"]],
  enemyIds: ["a1l1_en"],
  slots: 1,
};

export const a1l2: LevelData = {
  name: "A1 level2",
  id: "a1l2",
  icon: "icon_a1l2_327_142.png",
  iconLocation: { x: 1055, y: 395 },
  cardIds: [["a1l2_fr"], ["c_i"]],
  enemyIds: ["a1l2_en"],
  slots: 2,
};

export const a1l3: LevelData = {
  name: "A1 level3",
  id: "a1l3",
  icon: "icon_a1l3_215_165.png",
  iconLocation: { x: 1330, y: 175 },
  cardIds: [["d_i"], ["c_iii"]],
  enemyIds: ["a1l3_en1"],
  slots: 2,
};

/*export const a1l4: LevelData = {
  name: "A1 level4",
  id: "a1l4",
  icon: "icon_a1l4_207_160.png",
  iconLocation: { x: 820, y: 570 },
  cardIds: [["a1l4_fr1"], ["a1l4_fr2"]],
  enemyIds: ["a1l4_en1", "a1l4_en2"],
  slots: 2,
};*/

export const a1l5: LevelData = {
  name: "A1 level5",
  id: "a1l5",
  icon: "icon_a1l5_207_160.png",
  iconLocation: { x: 912, y: 800 },
  cardIds: [["a1l5_fr3"], ["a1l5_fr2"], ["a1l5_fr1"]],
  enemyIds: ["a1l5_en1", "a1l5_en2"],
  slots: 3,
};

export const a1l6: LevelData = {
  name: "A1 level6",
  id: "a1l6",
  icon: "icon_a1l6_183_103.png",
  iconLocation: { x: 1100, y: 620 },
  cardIds: [["a1l6_fr3"], ["a1l6_fr2"], ["a1l6_fr1"]],
  enemyIds: ["a1l6_en1", "a1l6_en2"],
  slots: 3,
};

/*export const a1l7: LevelData = {
  name: "A1 level7",
  id: "a1l7",
  icon: "icon_a1l7_183_103.png",
  iconLocation: { x: 1247, y: 800 },
  cardIds: [["a1l7_fr1"], ["a1l7_fr2"], ["a1l7_fr3"]],
  enemyIds: ["a1l7_en1"],
  slots: 3,
};*/

export const a1l8: LevelData = {
  name: "A1 level8",
  id: "a1l8",
  icon: "icon_a1l8_183_103.png",
  iconLocation: { x: 1380, y: 610 },
  cardIds: [["d_i"], ["c_i"]],
  enemyIds: ["a1l8_en1"],
  slots: 2,
};

export const a1l9: LevelData = {
  name: "A1 level9",
  id: "a1l9",
  icon: "icon_a1l4_207_160.png",
  iconLocation: { x: 820, y: 570 },
  cardIds: [["d_i"], ["c_ii"], ["u_i"]],
  enemyIds: ["a1l9_en1"],
  slots: 3,
};

export const a1l10: LevelData = {
  name: "A1 level10",
  id: "a1l10",
  icon: "icon_a1l7_183_103.png",
  iconLocation: { x: 1247, y: 800 },
  cardIds: [["a1l10_fr1"], ["c_iii"], ["u_i"]],
  enemyIds: ["a1l10_en1"],
  slots: 3,
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
  icon: "act_3_icon_100_100.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l1_en1"],
  slots: 3,
};

export const a3l2: LevelData = {
  name: "A3 level2",
  id: "a3l2",
  icon: "act_3_icon_100_100.png",
  iconLocation: { x: 400, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l2_en1"],
  slots: 3,
};

export const a3l2_1: LevelData = {
  name: "A3 level2 -1",
  id: "a3l2_1",
  icon: "act_2_icon_100_100.png",
  iconLocation: { x: 250, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l2_en1_1"],
  slots: 3,
};

export const a3l2_2: LevelData = {
  name: "A3 level2 -2",
  id: "a3l2_2",
  icon: "act_1_icon_100_100.png",
  iconLocation: { x: 100, y: 250 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l2_en1_2"],
  slots: 3,
};

export const a3l3: LevelData = {
  name: "A3 level3",
  id: "a3l3",
  icon: "act_3_icon_100_100.png",
  iconLocation: { x: 400, y: 450 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l3_en1", "a3l3_en2"],
  slots: 3,
};

export const a3l3_1: LevelData = {
  name: "A3 level3 -1",
  id: "a3l3_1",
  icon: "act_2_icon_100_100.png",
  iconLocation: { x: 250, y: 450 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l3_en1_1", "a3l3_en2_1"],
  slots: 3,
};

export const a3l3_2: LevelData = {
  name: "A3 level3 -2",
  id: "a3l3_2",
  icon: "act_1_icon_100_100.png",
  iconLocation: { x: 100, y: 450 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l3_en1_2", "a3l3_en2_2"],
  slots: 3,
};

export const a3l4: LevelData = {
  name: "A3 level4",
  id: "a3l4",
  icon: "act_3_icon_100_100.png",
  iconLocation: { x: 400, y: 650 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l4_en1"],
  slots: 3,
};

export const a3l4_1: LevelData = {
  name: "A3 level4 -1",
  id: "a3l4_1",
  icon: "act_2_icon_100_100.png",
  iconLocation: { x: 250, y: 650 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l4_en1_1"],
  slots: 3,
};

export const a3l4_2: LevelData = {
  name: "A3 level4 -2",
  id: "a3l4_2",
  icon: "act_1_icon_100_100.png",
  iconLocation: { x: 100, y: 650 },
  cardIds: [["d_i", "d_iii", "d_iv"], ["c_i", "c_ii", "c_iii"], ["u_i", "u_ii", "u_iii"]],
  enemyIds: ["a3l4_en1_2"],
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
  //"a1l4": a1l4,
  "a1l5": a1l5,
  "a1l6": a1l6,
  //"a1l7": a1l7,
  "a1l8": a1l8,
  "a1l9": a1l9,
  "a1l10": a1l10,
  "a2l1": a2l1,
  "a3l1": a3l1,
  "a3l2": a3l2,
  "a3l3": a3l3,
  "a3l4": a3l4,
  "a3l5": a3l5,
  "a3l2_1": a3l2_1,
  "a3l2_2": a3l2_2,
  "a3l3_1": a3l3_1,
  "a3l3_2": a3l3_2,
  "a3l4_1": a3l4_1,
  "a3l4_2": a3l4_2,
}
