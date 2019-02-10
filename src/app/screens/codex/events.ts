import { GameRefs } from "../../states/game";
import { loadCodexScreen } from "../menu/events";

export function changePage(
  gameRefs: GameRefs,
  type: { tag: "CardId", cardId: string },
) {
  gameRefs.screens.codexScreen.page = type;
  loadCodexScreen(gameRefs);
}