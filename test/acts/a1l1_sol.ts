import { mkGameState, showGamestate } from "../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution } from "../../src/shared/game/solution";
import { trinity_tnk } from "../../src/shared/data/act1/frUnits";
import { friendlyId } from "../../src/shared/definitions/entityId";

const initState = mkGameState(
  ["trinity_dmg", "trinity_tnk", "trinity_sup"],
  ["l1_en"],
);

console.log("-------------------------------------------");
console.log(showGamestate(initState));

const _sol0 = extendSolution({
  ability: trinity_tnk.abilities[0].ability,
  origin: friendlyId(1),
  inputs: [],
}, emptySolution, []);

const sol0 = runSolution(_sol0.solution, _sol0.loc, initState);

console.log("-------------------------------------------");
console.log(showGamestate(sol0.state));