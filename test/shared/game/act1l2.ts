import { mkGameState } from "../../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution } from "../../../src/shared/game/solution";
import * as Fr from "../../../src/shared/data/units/friendly";
import { PositionId, GlobalId } from "../../../src/shared/game/entityId";

const initState = mkGameState(
  ["fr_unit_a1_01", "fr_unit_a1_02", "fr_unit_a1_03"],
  ["en_unit_a1_l2_01"],
);

const _sol0 = extendSolution({
  ability: Fr.fr_unit_a1_l2_01.abilities[0],
  origin: new PositionId(0, "friendly"),
  inputs: [new PositionId(0, "enemy")],
}, emptySolution, []);

const _sol1 = extendSolution({
  ability: Fr.fr_unit_a1_l2_01.abilities[0],
  origin: new PositionId(0, "friendly"),
  inputs: [new PositionId(0, "enemy")],
}, _sol0.solution, _sol0.loc);

const sol = runSolution(_sol1.solution, _sol1.loc, initState);

console.log(JSON.stringify(sol.state));