import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["a1l1_fr"],
  ["a1l1_en"],
);

console.log(showGamestate(initState));

trySolutions(initState, 5);