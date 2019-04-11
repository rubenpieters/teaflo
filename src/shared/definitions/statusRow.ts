import { StatusId, UnitId } from "./entityId";
import deepEqual from "deep-equal";
import { Status } from "./status";

export type StStatus = Status & {
  id: StatusId,
  owner: UnitId,
}

export class StatusRow {
  constructor(
    public readonly statuses: StStatus[] = [],
  ) {}
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