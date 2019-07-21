import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

const initState = mkGameState(
  ["c_i", "d_i"],
  ["a1l8_en1"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);
