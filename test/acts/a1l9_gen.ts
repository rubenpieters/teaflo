import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

const initState = mkGameState(
  ["u_i", "c_ii", "d_i"],
  ["a1l9_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);
