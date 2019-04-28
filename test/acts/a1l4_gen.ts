import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["a1l4_fr1", "a1l4_fr2"],
  ["a1l4_en1", "a1l4_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 10);