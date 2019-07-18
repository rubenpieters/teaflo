import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["a1l5_fr1", "a1l5_fr2", "a1l5_fr3"],
  ["a1l5_en1", "a1l5_en2"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);