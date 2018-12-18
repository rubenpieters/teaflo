// act -> button string mapping
export const actNumberMap: { [key: number]: string } = {
  0: "1",
  1: "2",
};

// act -> level id mapping
export const levelMap: { [key: number]: string[] } = {
  0: ["a1_l1", "a1_l2"],
  1: ["a2_l1", "a2_l2", "a2_l3"],
}

type LevelData = {
  cardIds: string[],
  slots: number,
}

// level id -> card id mapping
export const levelDataMap: { [key: string]: LevelData } = {
  "a1_l1": { cardIds: ["fr_unit_a1_l1_01"], slots: 1 },
  "a1_l2": { cardIds: ["fr_unit_a1_l2_01", "fr_unit_a1_l2_02", "fr_unit_a1_l2_03"], slots: 3 },
  "a2_l1": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l2": { cardIds: ["card1", "card2", "card3"], slots: 4 },
  "a2_l3": { cardIds: ["card1", "card2", "card3"], slots: 4 },
}

// level id -> enemy unit id mapping
export const levelEnUnitMap: { [key: string]: (string | undefined)[] } = {
  "a1_l1": ["en_unit_a1_l1_01"],
  "a1_l2": ["en_unit_a1_l1_01", "en_unit_a1_l1_01"],
  "a2_l1": ["en_unit_a1_l1_01", "en_unit_a1_l1_01", "en_unit_a1_l1_01"],
  "a2_l2": ["card1"],
  "a2_l3": ["card1"],
}