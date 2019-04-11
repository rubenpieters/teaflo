import { UnitType, HasId } from "./entityId";

export class UnitRow<Type extends UnitType, E extends HasId<Type>> {
  constructor(
    public readonly type: Type,
    public readonly units: (E | undefined)[] = [],
  ) {}
}
