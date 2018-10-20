import { focus, over, set } from "src/shared/iassign-util";
import { GameState, PositionIdA, findEntity, CreatureId } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Origin } from "src/shared/game/target";

type Sim = {
    state: GameState,
    status: Status,
    input: { action: Action, origin: Origin }[],
  }

type Status
  = "WaitForInput"
  | "Finished"
  | "PhaseTurnAction"

function runSim(
  sim: Sim
) {
  switch (sim.status) {
    case "WaitForInput": {
      if (sim.input.length > 0) {
        return focus(sim,
          set(x => x.status, "PhaseTurnAction"),
          set(x => x.state.currentAction, sim.input[0]),
          over(x => x.input, x => x.slice(1)),
        );
      } else {
        return focus(sim,
          set(x => x.status, "Finished"),
        );
      }
    }
    case "PhaseTurnAction": {

    }
    case "Finished": {
      return sim;
    }
  }
}

export function phaseTransformCheckTransforms(
  state: GameState,
  id: CreatureId,
) {
  const e = findEntity(state, id);
  if (e.triggers.map(x => x.type).filter(x => x === "instead").length === 0) {
    // if entity has no transforms, skip to next entity
  } else {
    // if entity has transforms, start at transform index 0
  }
}

export function phaseTransformApplyTransform(
  action: Action,
  state: GameState,
  id: CreatureId,
  transformId: number,
) {
  const e = findEntity(state, id);
  
}
