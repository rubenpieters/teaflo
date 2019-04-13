import * as fc from "fast-check";
import { equalitySanityCheck, concatArbitrary } from "../../util/fast-check";
import { unitIdArb, statusIdArb } from "./entityId.spec";
import deepEqual from "deep-equal";
import { fragmentsArb, statusArb } from "./status.spec";
import { StatusRow, StStatus, statusRowInvariant } from "../../../src/shared/definitions/statusRow";
import { damageStatus, addStatus, removeStatus } from "../../../src/shared/game/statusRow";

const statusRowArb: fc.Arbitrary<StatusRow> = fc.set(statusIdArb, (a, b) => deepEqual(a, b))
  .chain(statusIdList => {
    const statusListArb = concatArbitrary(statusIdList.map(statusId => {
      return fc.record({ status: statusArb, ownerId: unitIdArb })
        .map(r => {
          const st: StStatus = {...r.status, id: statusId, owner: r.ownerId }
          return st;
        });
    }));
    const statusRowArb = statusListArb.map(x => new StatusRow(x));
    return statusRowArb;
  });

equalitySanityCheck(statusRowArb);
fc.assert(fc.property(statusRowArb, (row) => statusRowInvariant(row)));
fc.assert(fc.property(statusRowArb, statusIdArb, fragmentsArb,
  (row, statusId, value) => {
    const newRow = damageStatus(row, statusId, value);
    return statusRowInvariant(newRow) &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x =>
        ! deepEqual(x.id, statusId)), row.statuses.filter(x => ! deepEqual(x.id, statusId))
      );
  }));
fc.assert(fc.property(statusRowArb, statusArb, unitIdArb,
  (row, status, ownerId) => {
    const nextId = row.statuses.map(x => x.id.id).reduce((a,b) => a + b, 1);
    const newRow = addStatus(row, status, ownerId, nextId);
    return statusRowInvariant(newRow) &&
      // the new status was added
      newRow.statuses.filter(x => x.id.id === nextId).length ===
        row.statuses.filter(x => x.id.id === nextId).length + 1
      ;
  }));
fc.assert(fc.property(statusRowArb, statusIdArb,
  (row, statusId) => {
    const result = removeStatus(row, statusId);
    const newRow = result.row;

    const occurrences = row.statuses.filter(x => deepEqual(x.id, statusId));
    const removed: StStatus[] = result.entity === undefined ?
      [] : [result.entity];
    return statusRowInvariant(newRow) &&
      // no elements with this id are left
      newRow.statuses.filter(x => deepEqual(x.id, statusId)).length === 0 &&
      // other elements are unchanged
      deepEqual(newRow.statuses.filter(x =>
        ! deepEqual(x.id, statusId)), row.statuses.filter(x => ! deepEqual(x.id, statusId))
      ) &&
      // returned entity is equal to the removed entity
      deepEqual(occurrences, removed)
      ;
  }));
