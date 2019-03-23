import * as T from "../../game/trigger";
import { FrUnit } from "../../game/unit";
import * as I from "../../game/intent";
import { TargetInput, StatusInput, UnitInput } from "../../game/ability";
import { cost, thDamage } from "../intents/basic";

export const dmg_1: FrUnit = {
  hp: 1,
  maxHp: 1,
  charges: 2,
  maxCharges: 2,
  abilities: [
    {
      intent: cost(1,
          thDamage(new I.FromInput(0), new I.Static(1))
        ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "dmg_1",
    },
  ],
  vital: true,
};

export const armor_1: FrUnit = {
  hp: 1,
  maxHp: 1,
  charges: 2,
  maxCharges: 2,
  abilities: [
    {
      intent: cost(1,
          new I.AddTriggerI(
            new I.FromInput(0),
            new I.Static(T.full(new T.Armor(3))),
          ),
        ),
      inputs: [
        new UnitInput(),
      ],
      spriteId: "ab3",
      name: "armor_1",
    },
  ],
  vital: true,
};