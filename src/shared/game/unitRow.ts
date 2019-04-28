import { UnitType, EntityId, HasId } from "../definitions/entityId";
import deepEqual from "deep-equal";
import { focus, over, modifyAndGet } from "../iassign-util";
import { UnitRow } from "../definitions/unitRow";

export function defined<Type extends UnitType, E extends HasId<Type>>(
  row: UnitRow<Type, E>,
): { e: E, i: number }[] {
  const l = row.units
    .map((e, i) => { return { e, i }})
    .filter(r => r.e !== undefined)
    ;
  return l as { e: E, i: number }[];
}

export function overUnit<Type extends UnitType, E extends HasId<Type>>(
  row: UnitRow<Type, E>,
  id: EntityId<Type>,
  f: (e: E) => E,
): { row: UnitRow<Type, E>, entity?: E } {
  const index = row.units.findIndex(e => {
    if (e === undefined) return false;
    return deepEqual(e.id, id);
  });
  if (index === -1) {
    return { row };
  }
  const newRow = focus(row,
    over(x => x.units[index]!, f),
  );
  return { row: newRow, entity: newRow.units[index]! };
}

export function removeUnit<Type extends UnitType, E extends HasId<Type>>(
  row: UnitRow<Type, E>,
  id: EntityId<Type>,
): { row: UnitRow<Type, E>, entity?: E } {
  const index = row.units.findIndex(x => {
    if (x === undefined) return false;
    return deepEqual(x.id, id);
  });
  if (index === -1) {
    return { row };
  }

  const result = modifyAndGet(row,
    x => x.units, x => {
      const removed = x.splice(index, 1, undefined)[0];
      return { a: x, b: removed };
    }
  );
  return { row: result.s, entity: result.b };
}