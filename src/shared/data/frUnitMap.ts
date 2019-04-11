import { FrUnit } from "../definitions/unit";
import { Equal, isTrue } from "../type-util";
import { trinity_dmg, trinity_sup, trinity_tnk } from "./act1/frUnits";

// check that values of frUnitMap are all `FrUnit`
type FrUnitMapValues = (typeof frUnitMap)[keyof (typeof frUnitMap)];
isTrue<Equal<FrUnitMapValues, FrUnit>>(true);
export type FrUnitId = keyof (typeof frUnitMap);

export const frUnitMap = {
  "trinity_dmg": trinity_dmg,
  "trinity_sup": trinity_sup,
  "trinity_tnk": trinity_tnk,
}