import * as fc from "fast-check";
import { equalitySanityCheck, concatArbitrary } from "../../util/fast-check";
import { unitIdArb, statusIdArb } from "./entityId.spec";
import deepEqual from "deep-equal";
import { statusTags } from "../../../src/shared/game/status";
import { tagStatusArb, fragmentsArb, statusArb } from "./status.spec";
import { StatusRow, StStatus, statusRowInvariant } from "../../../src/shared/game/statusRow";

const statusRowArb = concatArbitrary(statusTags.map(tag => {
  const unitIdListArb =
    fc.set(unitIdArb, (a, b) => deepEqual(a, b))
  const statusListArb = unitIdListArb.chain(unitIdList => concatArbitrary(unitIdList.map(unitId => {
    return fc.record({ status: tagStatusArb(tag), id: statusIdArb })
    .map(r => {
      const st: StStatus = {...r.status, id: r.id, owner: unitId }
      return st;
    });
  })));
  return statusListArb;
})).map(l => {
  const flattened = l.reduce((acc, prev) => acc.concat(prev), []);
  return new StatusRow(flattened)
});

equalitySanityCheck(statusRowArb);
fc.assert(fc.property(statusRowArb, (row) => statusRowInvariant(row)));
fc.assert(fc.property(statusRowArb, statusIdArb, fragmentsArb,
  (row, statusId, value) => {
    const newRow = row.damageStatus(statusId, value);
    return statusRowInvariant(newRow) &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x => ! deepEqual(x.id, statusId)), row.statuses.filter(x => ! deepEqual(x.id, statusId)))
      ;
  }));
fc.assert(fc.property(statusRowArb, statusArb, unitIdArb,
  (row, status, ownerId) => {
    const nextId = row.statuses.map(x => x.id.id).reduce((a,b) => a + b, 0);
    const newRow = row.addStatus(status, ownerId, () => nextId);
    return statusRowInvariant(newRow) &&
      // the new status was added
      (newRow.statuses.filter(x => x.id.id === nextId).length === 1 ||
        // or, the status has 0 hp
        status.hp === 0 ||
        // or, the new status was merged into an existing status
        ! deepEqual(
          newRow.statuses.filter(x => x.tag === status.tag && deepEqual(x.owner, ownerId)),
          row.statuses.filter(x => x.tag === status.tag && deepEqual(x.owner, ownerId)),
        )
      )
      ;
  }));
fc.assert(fc.property(statusRowArb, statusIdArb,
  (row, statusId) => {
    const newRow = row.removeStatus(statusId);
    return statusRowInvariant(newRow) &&
      // no elements with this id are left
      newRow.statuses.filter(x => deepEqual(x.id, statusId)).length === 0 &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x => ! deepEqual(x.id, statusId)), row.statuses.filter(x => ! deepEqual(x.id, statusId)))
      ;
  }));