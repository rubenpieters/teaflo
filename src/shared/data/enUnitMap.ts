import { FrUnit, EnUnit } from "../definitions/unit";
import { Equal, isTrue } from "../type-util";
import { a2l1_en } from "./act2/level1";
import { a1l1_en } from "./act1/level1";
import { a1l2_en } from "./act1/level2";
import { a1l3_en1, a1l3_en2 } from "./act1/level3";
import { a1l4_en1, a1l4_en2 } from "./act1/level4";

// check that values of frUnitMap are all `FrUnit`
type EnUnitMapValues = (typeof enUnitMap)[keyof (typeof enUnitMap)];
isTrue<Equal<EnUnitMapValues, EnUnit>>(true);
export type EnUnitId = keyof (typeof enUnitMap);

export const enUnitMap = {
  "a1l1_en": a1l1_en,
  "a1l2_en": a1l2_en,
  "a1l3_en1": a1l3_en1,
  "a1l3_en2": a1l3_en2,
  "a1l4_en1": a1l4_en1,
  "a1l4_en2": a1l4_en2,
  "a2l1_en": a2l1_en,
}