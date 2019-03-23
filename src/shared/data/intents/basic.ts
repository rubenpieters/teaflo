import { FrUnit } from "../../game/unit";
import * as I from "../../game/intent";
import { TargetInput, StatusInput, UnitInput } from "../../game/ability";
import * as T from "../../game/trigger";
import { UnitId } from "src/shared/game/entityId";

export function thDamage(
  target: I.IntentVar<UnitId>,
  value: I.IntentVar<number>,
): I.Intent {
  return new I.CombinedIntent([
    new I.DamageI(target, value),
    new I.AddThreatI(I.mkSelf(), target, value),
  ]);
}

export function cost(
  costAmt: number,
  intent: I.Intent,
): I.Intent {
  return new I.CombinedIntent([
    new I.UseChargeI(
      I.mkSelf(),
      new I.Static(costAmt),
    ),
    intent,
  ]);
}