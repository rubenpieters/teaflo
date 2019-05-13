import { FrUnit } from "../definitions/unit";
import { Equal, isTrue } from "../type-util";
import { trinity_dmg, trinity_sup, trinity_tnk } from "./act2/frUnits";
import { a1l1_fr } from "./act1/level1";
import { a1l2_fr } from "./act1/level2";
import { a1l3_fr1, a1l3_fr2 } from "./act1/level3";
import { a1l4_fr1, a1l4_fr2 } from "./act1/level4";
import { a1l5_fr1, a1l5_fr2, a1l5_fr3 } from "./act1/level5";

// check that values of frUnitMap are all `FrUnit`
type FrUnitMapValues = (typeof frUnitMap)[keyof (typeof frUnitMap)];
isTrue<Equal<FrUnitMapValues, FrUnit>>(true);
export type FrUnitId = keyof (typeof frUnitMap);

export const frUnitMap = {
  "a1l1_fr": a1l1_fr,
  "a1l2_fr": a1l2_fr,
  "a1l3_fr1": a1l3_fr1,
  "a1l3_fr2": a1l3_fr2,
  "a1l4_fr1": a1l4_fr1,
  "a1l4_fr2": a1l4_fr2,
  "a1l5_fr1": a1l5_fr1,
  "a1l5_fr2": a1l5_fr2,
  "a1l5_fr3": a1l5_fr3,
  "trinity_dmg": trinity_dmg,
  "trinity_sup": trinity_sup,
  "trinity_tnk": trinity_tnk,
}