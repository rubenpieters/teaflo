import { UnitType, EntityId, HasId } from "./entityId";
import deepEqual from "deep-equal";
import { focus, over } from "../iassign-util";

export class UnitRow<Type extends UnitType, E extends HasId> {
  constructor(
    public readonly type: Type,
    public readonly units: (E | undefined)[] = [],
  ) {}

  overUnit(
    id: EntityId<Type>,
    f: (e: E) => E,
  ): UnitRow<Type, E> {
    const index = this.units.findIndex(e => {
      if (e === undefined) return false;
      return deepEqual(e.id, id);
    });
    if (index === -1) {
      return this;
    }
    return focus(this,
      over(x => x.units[index]!, f),
    );
  }
}
