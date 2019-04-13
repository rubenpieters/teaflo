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
 * The invariant which should hold for all StatusRows.
 * 
 * A status id should be unique.
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