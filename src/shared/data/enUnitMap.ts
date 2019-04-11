import { FrUnit, EnUnit } from "../definitions/unit";
import { Equal, isTrue } from "../type-util";
import { l1_en } from "./act1/level1";

// check that values of frUnitMap are all `FrUnit`
type EnUnitMapValues = (typeof enUnitMap)[keyof (typeof enUnitMap)];
isTrue<Equal<EnUnitMapValues, EnUnit>>(true);
export type EnUnitId = keyof (typeof enUnitMap);

export const enUnitMap = {
  "l1_en": l1_en,
}