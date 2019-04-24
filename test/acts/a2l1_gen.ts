import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["trinity_dmg", "trinity_tnk", "trinity_sup"],
  ["a2l1_en"],
);

console.log(showGamestate(initState));

trySolutions(initState, 10);