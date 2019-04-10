import { focus, over, modifyAndGet } from "../iassign-util";
import { Status } from "./status";
import { StatusId, UnitId, HasId, statusId } from "./entityId";
import deepEqual from "deep-equal";
import { damageEntity } from "./entity";

export type StStatus = Status & {
  id: StatusId,
  owner: UnitId,
}

export class StatusRow {
  constructor(
    public readonly statuses: StStatus[] = [],
  ) {}

  damageStatus(
    statusId: StatusId,
    value: number,
  ): StatusRow {
    return this.overStatus(statusId, status => damageEntity(status, value)).row;
  }

  overStatus(
    id: StatusId,
    f: (e: StStatus) => StStatus,
  ): { row: StatusRow, status?: StStatus } {
    const index = this.statuses.findIndex(e => {
      if (e === undefined) return false;
      return deepEqual(e.id, id);
    });
    if (index === -1) {
      return { row: this };
    }
    const row = new StatusRow(focus(this.statuses,
      over(x => x[index]!, f),
    ));
    return { row, status: row.statuses[index]! };
  }

  removeStatus(
    statusId: StatusId,
  ): { row: StatusRow, entity?: StStatus } {
    const index = this.statuses.findIndex(x => deepEqual(x.id, statusId));
    if (index === -1) {
      return { row: this };
    }

    const result = modifyAndGet(this,
      x => x.statuses, x => {
        const removed = x.splice(index, 1)[0];
        return { a: x, b: removed };
      }
    );
    return { row: result.s, entity: result.b };
  }

  addStatus(
    status: Status,
    ownerId: UnitId,
    nextId: number,
  ): StatusRow {
    const newId = statusId(nextId);
    const newStatus: StStatus =
      {...status, owner: ownerId, id: newId };
    return new StatusRow(focus(this.statuses,
      over(x => x, x => x.concat(newStatus)),
    ));
  }

  statusPosition(
    statusId: StatusId,
  ): number | undefined {
    const index = this.statuses.findIndex(x => deepEqual(x.id, statusId));
    if (index === -1) return undefined;
    return index;
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
    const amount = row.statuses.filter(x => deepEqual(x.id, status.id)).length;
    // status ids should be unique
    if (amount !== 1) return false;
  }
  return true;
}