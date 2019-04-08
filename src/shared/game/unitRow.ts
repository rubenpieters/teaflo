import { UnitType, EntityId, HasId } from "./entityId";
import deepEqual from "deep-equal";
import { focus, over, modifyAndGet } from "../iassign-util";

export class UnitRow<Type extends UnitType, E extends HasId<Type>> {
  constructor(
    public readonly type: Type,
    public readonly units: (E | undefined)[] = [],
  ) {}

  defined(): { e: E, i: number }[] {
    const l = this.units
      .map((e, i) => { return { e, i }})
      .filter(r => r.e !== undefined)
      ;
    return l as { e: E, i: number }[];
  }

  overUnit(
    id: EntityId<Type>,
    f: (e: E) => E,
  ): { row: UnitRow<Type, E>, entity?: E } {
    const index = this.units.findIndex(e => {
      if (e === undefined) return false;
      return deepEqual(e.id, id);
    });
    if (index === -1) {
      return { row: this };
    }
    const row = focus(this,
      over(x => x.units[index]!, f),
    );
    return { row, entity: row.units[index]! };
  }

  removeUnit(
    id: EntityId<Type>,
  ): { row: UnitRow<Type, E>, entity?: E } {
    const index = this.units.findIndex(x => {
      if (x === undefined) return false;
      return deepEqual(x.id, id);
    });
    if (index === -1) {
      return { row: this };
    }

    const result = modifyAndGet(this,
      x => x.units, x => {
        const removed = x.splice(index, 1)[0];
        return { a: x, b: removed };
      }
    );
    return { row: result.s, entity: result.b };
  }
}
