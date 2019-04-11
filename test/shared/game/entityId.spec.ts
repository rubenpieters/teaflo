import * as fc from "fast-check";
import { UnitType, unitTypes, UnitId, EntityId, TargetId, targetTypes, TargetType, StatusId } from "../../../src/shared/definitions/entityId";
import { equalitySanityCheck } from "../../util/fast-check";


// TODO: customizable to generate ids in/out a certain set with a given chance

const idNrArb: fc.Arbitrary<number> =
  fc.integer(0, 100);
const unitTypeArb: fc.Arbitrary<UnitType> =
  fc.constantFrom(...unitTypes);
const targetTypeArb: fc.Arbitrary<TargetType> =
  fc.constantFrom(...targetTypes);

export const unitIdArb: fc.Arbitrary<UnitId> =
  fc.record({ id: idNrArb, type: unitTypeArb }).map(r => new EntityId(r.id, r.type));
export const statusIdArb: fc.Arbitrary<StatusId> =
  fc.record({ id: idNrArb }).map(r => new EntityId(r.id, "status"));
export const targetIdArb: fc.Arbitrary<TargetId> =
  fc.record({ id: idNrArb, type: targetTypeArb }).map(r => new EntityId(r.id, r.type));

equalitySanityCheck(unitIdArb);
equalitySanityCheck(statusIdArb);
equalitySanityCheck(targetIdArb);