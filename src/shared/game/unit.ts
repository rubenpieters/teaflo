import { focus, over } from "../iassign-util";
import { moveAI } from "./ai";
import { Unit, EnUnit } from "../definitions/unit";
import { AIDirection } from "../definitions/ai";

export function useChargeUnit<E extends Unit>(
  e: E,
  value: number,
): E {
  return focus(e,
    over(x => x.charges, x => x - value),
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