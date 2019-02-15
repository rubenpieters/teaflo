import * as A from "../../game/action";
import { AI} from "../../game/ai";
import * as O  from "../../game/ai";
import { GameState } from "../../game/state";
import * as I from "../../game/intent";
import { PositionId, UnitId, getUnit } from "../../game/entityId";
import * as T from "../../game/trigger";

export const ai1: AI = [
    {
      intent: new I.DamageI(
        new I.Static(new PositionId(0, "friendly")),
        new I.Static(2),
      ),
      spriteId: "fr_unit_a1_l1_01_ab1",
      outs: [
          {
            aiOut: new O.ToSelf(),
            condition: (state: GameState) => true,
          },
        ],
    },
  ];

export const ai2: AI = [
    {
      intent: new I.AddTriggerI(
        I.mkAllAlly(),
        new I.Static(
          T.full(new T.Weak(1)),
        ),
      ),
      spriteId: "fr_unit_a1_l2_01_ab1",
      outs: [
          {
            aiOut: new O.ToX(1),
            condition: (state: GameState) => true,
          },
        ],
    },
    {
      intent: new I.DamageI(
        I.mkHighestThreat(),
        new I.Static(15),
      ),
      spriteId: "fr_unit_a1_l1_01_ab1",
      outs: [
          {
            aiOut: new O.ToX(0),
            condition: (state: GameState) => true,
          },
        ],
    },
  ];


export const ai3: AI = [
  {
    intent: new I.AddTriggerI(
      I.mkSelf(),
      new I.Static(
        new T.Grow(1, new T.Strong(1)),
      ),
    ),
    spriteId: "fr_unit_a1_l2_01_ab1",
    outs: [
        {
          aiOut: new O.ToX(1),
          condition: (state: GameState, selfId: UnitId) => {
            const selfUnit = getUnit(selfId, state);
            if (selfUnit !== undefined) {
              return selfUnit.hp < selfUnit.maxHp;
            }
            throw "Should not happen";
          },
        },
        {
          aiOut: new O.ToSelf,
          condition: (state: GameState) => true,
        },
      ],
  },
  {
    intent: new I.DamageI(
      I.mkHighestThreat(),
      new I.Static(5),
    ),
    spriteId: "fr_unit_a1_l1_01_ab1",
    outs: [
        {
          aiOut: new O.ToSelf(),
          condition: (state: GameState) => true,
        },
      ],
  },
];


export const ai4: AI = [
  {
    intent: new I.AddTriggerI(
      I.mkAllAlly(),
      new I.Static(
        new T.Weak(3),
      ),
    ),
    spriteId: "fr_unit_a1_l2_01_ab1",
    outs: [
        {
          aiOut: new O.ToSelf(),
          condition: (state: GameState) => true,
        },
      ],
  },
];

export const ai5: AI = [
  {
    intent: new I.AddTriggerI(
      I.mkHighestThreat(),
      new I.Static(
        T.full(new T.Fragile(4)),
      ),
    ),
    spriteId: "fr_unit_a1_l2_01_ab1",
    outs: [
        {
          aiOut: new O.ToX(1),
          condition: (state: GameState) => true,
        },
      ],
  },
  {
    intent: new I.DamageI(
      I.mkHighestThreat(),
      new I.Static(1),
    ),
    spriteId: "fr_unit_a1_l1_01_ab1",
    outs: [
        {
          aiOut: new O.ToX(0),
          condition: (state: GameState) => true,
        },
      ],
  },
];