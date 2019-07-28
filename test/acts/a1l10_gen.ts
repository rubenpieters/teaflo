import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

const initState = mkGameState(
  ["u_i", "c_iii", "a1l10_fr1"],
  ["a1l10_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 15);
