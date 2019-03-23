import * as T from "../../game/trigger";
import { FrUnit } from "../../game/unit";
import * as I from "../../game/intent";
import { TargetInput, StatusInput, UnitInput } from "../../game/ability";
import { cost, thDamage } from "../intents/basic";

export const trinity_dmg: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: cost(1,
          thDamage(new I.FromInput(0), new I.Static(10))
        ),
      inputs: [
        new TargetInput(),
      ],
      spriteId: "ab3",
      name: "dmg10",
    },
  ],
  vital: true,
};

export const trinity_tank: FrUnit = {
  hp: 11,
  maxHp: 11,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: cost(1,
        new I.AddThreatI(
          I.mkSelf(),
          I.mkAllEnemy(),
          new I.Static(13),
        ),
      ),
      inputs: [],
      spriteId: "ab1",
      name: "th13_all",
    },
  ],
  vital: true,
};

export const trinity_support: FrUnit = {
  hp: 5,
  maxHp: 5,
  charges: 5,
  maxCharges: 5,
  abilities: [
    {
      intent: cost(1,
        new I.AddTriggerI(
          new I.FromInput(0),
          new I.Static(T.full(new T.Armor(10))),
        ),
      ),
      inputs: [
        new UnitInput(),
      ],
      spriteId: "ab2",
      name: "armor10",
    },
  ],
  vital: true,
};