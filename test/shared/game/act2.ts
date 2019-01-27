import { mkGameState } from "../../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution } from "../../../src/shared/game/solution";
import * as Fr from "../../../src/shared/data/units/friendly";
import { PositionId, GlobalId } from "../../../src/shared/game/entityId";

const initState = mkGameState(
  ["fr_unit_a2_01", "fr_unit_a2_02"],
  ["en_unit_a2_l1_01", "en_unit_a2_l1_01", "en_unit_a2_l1_01"],
);

const sol0 = emptySolution;

const _sol1 = extendSolution({
  ability: Fr.fr_unit_a2_02.abilities[0],
  origin: new PositionId(1, "friendly"),
  inputs: [new PositionId(0, "friendly")],
}, sol0, []);

const sol1 = runSolution(_sol1.solution, _sol1.loc, initState);

console.log(JSON.stringify(sol1.state));