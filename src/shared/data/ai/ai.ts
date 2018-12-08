import * as A from "../../game/action";
import { AI, ToSelf } from "../../game/ai";
import { GameState } from "../../game/state";

export const ai1: AI = [
    {
      action: new A.Damage({ tag: "PositionId", type: "friendly", id: 0 }, 2),
      outs: [
          {
            aiOut: new ToSelf(),
            condition: (state: GameState) => true,
          },
        ],
    },
  ];