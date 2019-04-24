import { FrUnit, EnUnit } from "../definitions/unit";
import { Equal, isTrue } from "../type-util";
import { a2l1_en } from "./act2/level1";
import { a1l1_en } from "./act1/level1";

// check that values of frUnitMap are all `FrUnit`
type EnUnitMapValues = (typeof enUnitMap)[keyof (typeof enUnitMap)];
isTrue<Equal<EnUnitMapValues, EnUnit>>(true);
export type EnUnitId = keyof (typeof enUnitMap);

export const enUnitMap = {
  "a2l1_en": a2l1_en,
  "a1l1_en": a1l1_en,
}