import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";
import { pre } from "fast-check";

const initState = mkGameState(
  ["a1l6_fr1", "a1l6_fr2", "a1l6_fr3"],
  ["a1l6_en1", "a1l6_en2"],
);

console.log(showGamestate(initState));

trySolutions(initState, 13);
