import { EnUnitId } from "src/shared/data/enUnitMap";
import { FrUnitId } from "src/shared/data/frUnitMap";

export const cardMap: {
  [K in FrUnitId | EnUnitId]: string
} = {
  // act 1 level 1
  "a1l1_fr": "unit3.png",
  "a1l1_en": "unit4.png",
  "a1l2_fr": "unit3.png",
  "a1l2_en": "unit4.png",
  "a1l3_fr1": "unit3.png",
  "a1l3_fr2": "unit3.png",
  "a1l3_en1": "unit4.png",
  "a1l3_en2": "unit4.png",
  // act 2
  "trinity_dmg": "unit3.png",
  "trinity_tnk": "unit2.png",
  "trinity_sup": "unit1.png",
  // act 2 level 1
  "a2l1_en": "unit4.png",
}