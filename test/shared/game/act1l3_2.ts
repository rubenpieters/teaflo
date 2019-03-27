import { mkGameState, possibleActions } from "../../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution, showSolDataCompact } from "src/shared/game/solution";
import * as Fr from "src/shared/data/units/friendly";
import { PositionId, GlobalId } from "src/shared/game/entityId";
import { trySolutions } from "../../util";

const initState = mkGameState(
  ["fr_unit_a1_l2_03", "fr_unit_a1_l2_02", "fr_unit_a1_l2_01"],
  ["en_unit_a1_l3_01_2", "en_unit_a1_l3_02_2"],
);

trySolutions(initState, 7);