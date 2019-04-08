import * as fc from "fast-check";
import { equalitySanityCheck } from "../../util/fast-check";
import { Action } from "../../../src/shared/game/action";
import * as A from "../../../src/shared/game/action";
import { targetIdArb, unitIdArb } from "./entityId.spec";

export const actionTagArb: fc.Arbitrary<Action["tag"]> = fc.constantFrom(...A.actionTags);

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