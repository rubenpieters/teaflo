import * as fc from "fast-check";
import { StatusTag, Status } from "../../../src/shared/definitions/status";
import * as S from "../../../src/shared/definitions/status";
import { statusTags, showStatus, statusDescription } from "../../../src/shared/game/status";
import { sampleShow } from "../../util/fast-check";

export const statusTagArb: fc.Arbitrary<StatusTag> = fc.constantFrom(...statusTags);

export const fragmentsArb: fc.Arbitrary<number> = fc.integer(0, 100000);

export const statusArb: fc.Arbitrary<Status> = statusTagArb.chain(tag => {
  return tagStatusArb(tag)
});

export function tagStatusArb(
  tag: StatusTag,
): fc.Arbitrary<Status> {
  switch (tag) {
    case "Strong": return fragmentsArb.map(x => new S.Strong(x));
    case "Weak": return fragmentsArb.map(x => new S.Weak(x));
    case "Armor": return fragmentsArb.map(x => new S.Armor(x));
    case "Fragile": return fragmentsArb.map(x => new S.Fragile(x));
  }
}

sampleShow(statusArb, statusDescription);