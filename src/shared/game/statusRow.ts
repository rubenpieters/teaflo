import { focus, over } from "../iassign-util";
import { Status, statusMergeType } from "./status";
import { StatusId, UnitId, HasId, statusId } from "./entityId";
import deepEqual from "deep-equal";

export type StStatus = Status & {
  id: StatusId,
  owner: UnitId,
}

export class StatusRow {
  constructor(
    public readonly statuses: StStatus[] = [],
  ) {}

  damageFragments(
    statusId: StatusId,
    value: number,
  ): StatusRow {
    const index = this.statuses.findIndex(x => deepEqual(x.id, statusId));
    if (index === -1) {
      // if not found, damage fizzles
      return this;
    }
    // reduce the fragment value of found index
    return focus(this,
      over(x => x.statuses[index].fragments, x => Math.max(0, x - value)),
    );
  }

  removeStatus(
    statusId: StatusId,
  ) {
    return focus(this,
      over(x => x.statuses,
        x => x.filter(x => ! deepEqual(x.id, statusId))),
    );
  }

  addStatus(
    status: Status,
    ownerId: UnitId,
    nextId: () => number,
  ) {
    switch (statusMergeType(status)) {
      case "on_owner_id": {
        // merging on owner id
        const index = this.statuses.findIndex(x => deepEqual(x.owner, ownerId) && x.tag === status.tag);
        if (index === -1) {
          // if not found, create new status in row
          const newId = statusId(nextId());
          const newStatus: StStatus =
            {...status, owner: ownerId, id: newId };
          return focus(this,
              over(x => x.statuses, x => x.concat(newStatus))
            );
        } {
          // if found, add the fragment values to the existing status
          return focus(this,
            over(x => x.statuses[index].fragments,
              x => Math.max(0, x + status.fragments)),
          );
        }
      }
    }
  }
}

/**
 * An invariant which should hold for all StatusRows.
 * 
 * For the "on_owner_id" merge type:
 *   - Only one status with the same id and owner id exists
 */
export function statusRowInvariant(
  row: StatusRow,
): boolean {
  for (const status of row.statuses) {
    switch (statusMergeType(status)) {
      case "on_owner_id": {
        const diff = row.statuses.filter(x => {
          return deepEqual(x.owner, status.owner) && x.tag === status.tag
        });
        // we expect only one status with
        // the same owner and tag: itself
        if (diff.length !== 1) return false;
      }
    }
  }
  return true;
}