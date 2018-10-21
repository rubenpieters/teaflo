import { focus, over, set } from "src/shared/iassign-util";
import { GameState, PositionIdA, findEntity, CreatureId } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Origin } from "src/shared/game/target";
import { transformToTrigger, Transform } from "./status";

type Sim = {
    state: GameState,
    status: Status,
    input: { action: Action, origin: Origin }[],
  }

type WaitForInput = {
  tag: "WaitForInput",
}

type Finished = {
  tag: "Finished",
}

type TransformPhase = {
  tag: "TransformPhase",
  index: PositionIdA<"ally" | "enemy"> | undefined,
}

type TriggerPhase = {
  tag: "TriggerPhase",
  index: PositionIdA<"ally" | "enemy"> | undefined,
}

type Status
  = WaitForInput
  | Finished
  | TransformPhase
  | TriggerPhase
  ;

function runSim(
  sim: Sim
) {
  switch (sim.status.tag) {
    case "WaitForInput": {
      if (sim.input.length > 0) {
        return focus(sim,
          set(x => x.status, { tag: "TransformPhase", index: undefined }),
          set(x => x.state.currentAction, sim.input[0]),
          over(x => x.input, x => x.slice(1)),
        );
      } else {
        return focus(sim,
          set(x => x.status, { tag: "Finished" }),
        );
      }
      throw "";
    }
    case "TransformPhase": {
      throw "";
    }
    case "TriggerPhase": {
      throw "";
    }
    case "Finished": {
      return sim;
    }
  }
}

export function phaseTransformCheckTransforms(
  state: GameState,
  selfId: PositionIdA<"ally" | "enemy">,
) {
  const e = findEntity(state, selfId);
  if (e.transforms.length === 0) {
    // if entity has no transforms, skip to next entity
  } else {
    // if entity has transforms, start at transform index 0
  }
}

function nextTransformCheck(
  state: GameState,
  id: PositionIdA<"ally" | "enemy">,
): Status {
  let nextId = { ...id, id: id.id + 1 };
  if (nextId.type === "ally") {
    if (nextId.id >= state.crew.length) {
      nextId = {
        tag: "PositionId",
        type: "enemy",
        id: 0,
      }
    } else {
      return { tag: "TransformPhase", index: nextId };
    }
  }
  
  if (nextId.type === "enemy") {
    if (nextId.id >= state.enemies.length) {
      return { tag: "TriggerPhase", index: undefined };
    }
  }
  return { tag: "TransformPhase", index: nextId };
}

export function phaseTransformApplyTransform(
  trigger: Action,
  origin: Origin,
  state: GameState,
  selfId: CreatureId,
  transformId: number,
) {
  const e = findEntity(state, selfId);
  const transform = e.transforms[transformId];
  const triggerEff = transformToTrigger(transform.tag);
  const effect = triggerEff.effect({ state, selfId, trigger, transform, triggerOrigin: origin });
  const newAction = effect.action.tag === "Noop" ?
    trigger : effect.action;
  
  
}
