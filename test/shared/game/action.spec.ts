import * as fc from "fast-check";
import { equalitySanityCheck } from "../../util/fast-check";
import { Action } from "../../../src/shared/definitions/action";
import * as A from "../../../src/shared/definitions/actionf";
import { targetIdArb, unitIdArb } from "./entityId.spec";
import { actionTags } from "src/shared/game/action";

export const actionTagArb: fc.Arbitrary<Action["tag"]> = fc.constantFrom(...actionTags);

export const actionArb: fc.Arbitrary<Action> = actionTagArb.chain(tag => {
  return tagActionArb(tag)
});

export function tagActionArb(
  tag: Action["tag"],
): fc.Arbitrary<Action> {
  switch (tag) {
    case "Damage": {
      return fc.record({ value: fc.integer(0, 100), target: targetIdArb })
        .map(r => new A.Damage("Action", "Action", r.value, r.target));
    }
    case "UseCharge": {
      return fc.record({ value: fc.integer(0, 100), target: unitIdArb })
        .map(r => new A.UseCharge("Action", "Action", r.value, r.target));
    }
    default: {
      throw "unimpl";
    }
  }
}

equalitySanityCheck(actionArb);