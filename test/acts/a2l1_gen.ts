import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { trySolutions } from "../util";

const initState = mkGameState(
  ["trinity_sup", "trinity_dmg", "trinity_tnk"],
  ["a2l1_en"],
);

console.log(showGamestate(initState));

trySolutions(initState, 10);