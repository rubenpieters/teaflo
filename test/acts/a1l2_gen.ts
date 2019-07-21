import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["c_i", "a1l2_fr"],
  ["a1l2_en"],
);

console.log(showGamestate(initState));

trySolutions(initState, 5);