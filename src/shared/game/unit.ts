import { focus, over } from "../iassign-util";
import { moveAI } from "./ai";
import { Unit, EnUnit } from "../definitions/unit";
import { AIDirection } from "../definitions/ai";
import { UnitType, HasId } from "../definitions/entityId";
import { showEntityId } from "./entityId";

export function useChargeUnit<E extends Unit>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.charges, x => x - value),
  );
}

export function restoreChargeUnit<E extends Unit>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.charges, x => Math.min(e.maxCharges, x + value)),
  );
}

export function moveAIUnit<E extends EnUnit>(
  e: E,
  dir: AIDirection,
): E {
  return focus(e,
    over(x => x.aiPosition, x => moveAI(x, dir)),
  );
}

export function showUnit<Type extends UnitType>(
  unit: Unit & HasId<Type>,
  i: number,
) {
  return `${i}:${showEntityId(unit.id)} - ${unit.hp}/${unit.maxHp} - ${unit.charges}/${unit.maxCharges}`;
}