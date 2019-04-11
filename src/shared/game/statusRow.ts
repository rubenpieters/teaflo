import { focus, over, modifyAndGet } from "../iassign-util";
import { StatusId, UnitId, HasId, statusId } from "../definitions/entityId";
import deepEqual from "deep-equal";
import { damageEntity } from "./entity";
import { Status } from "../definitions/status";
import { StatusRow, StStatus } from "../definitions/statusRow";

export function damageStatus(
  row: StatusRow,
  statusId: StatusId,
  value: number,
): StatusRow {
  return overStatus(row, statusId, status => damageEntity(status, value)).row;
}

export function overStatus(
  row: StatusRow,
  id: StatusId,
  f: (e: StStatus) => StStatus,
): { row: StatusRow, status?: StStatus } {
  const index = row.statuses.findIndex(e => {
    if (e === undefined) return false;
    return deepEqual(e.id, id);
  });
  if (index === -1) {
    return { row };
  }
  const newRow = focus(row,
    over(x => x.statuses[index]!, f),
  );
  return { row: newRow, status: newRow.statuses[index]! };
}

export function removeStatus(
  row: StatusRow,
  statusId: StatusId,
): { row: StatusRow, entity?: StStatus } {
  const index = row.statuses.findIndex(x => deepEqual(x.id, statusId));
  if (index === -1) {
    return { row };
  }

  const result = modifyAndGet(row,
    x => x.statuses, x => {
      const removed = x.splice(index, 1)[0];
      return { a: x, b: removed };
    }
  );
  return { row: result.s, entity: result.b };
}

export function addStatus(
  row: StatusRow,
  status: Status,
  ownerId: UnitId,
  nextId: number,
): StatusRow {
  const newId = statusId(nextId);
  const newStatus: StStatus =
    {...status, owner: ownerId, id: newId };
  return focus(row,
    over(x => x.statuses, x => x.concat(newStatus)),
  );
}

export function statusPosition(
  row: StatusRow,
  statusId: StatusId,
): number | undefined {
  const index = row.statuses.findIndex(x => deepEqual(x.id, statusId));
  if (index === -1) return undefined;
  return index;
}