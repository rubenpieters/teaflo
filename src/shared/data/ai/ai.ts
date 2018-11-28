import { mkDamage } from "../../game/action";
import { AI, mkToSelf } from "../../game/ai";
import { GameState } from "../../game/state";

export const ai1: AI = [
    {
      action: mkDamage({ tag: "PositionId", type: "friendly", id: 0 }, 2),
      outs: [
          {
            aiOut: mkToSelf(),
            condition: (state: GameState) => true,
          },
        ],
    },
  ];