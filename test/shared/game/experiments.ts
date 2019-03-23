import { mkGameState, possibleActions, showStateCompact } from "../../../src/shared/game/state";
import { extendSolution, emptySolution, runSolution, showSolDataCompact } from "../../../src/shared/game/solution";
import * as Fr from "../../../src/shared/data/units/friendly";
import { PositionId, GlobalId } from "../../../src/shared/game/entityId";
import { trySolutions } from "../../util";

const initState = mkGameState(
  ["arm1", "dmg1"],
  ["exp1"],
);

console.log(showStateCompact(initState));

trySolutions(initState, 6);