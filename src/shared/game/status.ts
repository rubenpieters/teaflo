import * as C from "../definitions/condition";
import * as ST from "../definitions/statusTransform";
import { StatusLog } from "./log";
import { StStatus } from "../definitions/statusRow";
import { Status, StatusGroup, StatusTag } from "../definitions/status";
import { Damage, Death } from "../definitions/actionf";
import { ActionWithOrigin } from "../definitions/action";
import { GameState } from "../definitions/state";
import { resolveCondition } from "./condition";


export function statusGroup(
  status: Status,
) {
  switch (status.tag) {
    case "Armor": return "def_mod";
    case "Fragile": return "def_mod";
    case "Strong": return "atk_mod";
    case "Weak": return "atk_mod";
  }
}

export const groupOrder: StatusGroup[]
  = ["atk_mod", "def_mod"];

export const statusTags: StatusTag[]
  = ["Weak", "Strong", "Armor", "Fragile"];

export function statusToCondition(
  status: Status,
): C.ActionCondition {
  switch (status.tag) {
    case "Fragile":
    case "Armor": {
      // Dmg (Var 1) (StatusOwner) & (Var 2)
      return {...new Damage("Cond", "Cond", new C.Var("1"), C.statusOwner()), origin: new C.Var("2") };
    }
    case "Strong":
    case "Weak": {
      // Dmg (Var 1) (Var 2) & (StatusOwner)
      return {...new Damage("Cond", "Cond", new C.Var("1"), new C.Var("2")), origin: C.statusOwner() };
    }
  }
}

export function statusToTransform(
  status: Status,
): {
  transform: ST.StatusTransform,
  actions: ST.StatusTransform[],
} {
  switch (status.tag) {
    case "Armor": {
      const transform: ST.StatusTransform =
        {
          ...new Damage("ST", "ST", ST.monus(new C.Var("1"), C.statusValue()), C.statusOwner()),
          origin: new C.Var("3"),
        };
      const actions: ST.StatusTransform[] = [
        {
          ...new Death("ST", C.idOfStatus()),
          origin: new C.Static("noOrigin") as C.Static<"noOrigin">
        }
      ];
      return { transform, actions };
    }
    case "Weak": {
      const transform: ST.StatusTransform =
        {
          ...new Damage("ST", "ST", ST.monus(new C.Var("1"), C.statusValue()), new C.Var("2")),
          origin: C.statusOwner(),
        };
      const actions: ST.StatusTransform[] = [
      ];
      return { transform, actions };
    }
    default: {
      throw "unimpl";
    }
  }
}

export function applyStatuses(
  onStackAction: ActionWithOrigin,
  state: GameState,
): {
  transformed: ActionWithOrigin,
  actions: ActionWithOrigin[],
  transforms: StatusLog[],
} {
  let newActions: ActionWithOrigin[] = [];
  let transforms: StatusLog[] = [];
  for (const group of groupOrder) {
    for (const status of state.statusRows[group].statuses) {
      const { transformed, actions, statusLog } = applyStatus(status, onStackAction);
      onStackAction = transformed;
      newActions = newActions.concat(actions);
      if (statusLog !== undefined) {
        transforms = transforms.concat(statusLog);
      }
    }
  }
  return {
    actions: newActions,
    transformed: onStackAction,
    transforms,
  };
}

export function applyStatus(
  status: StStatus,
  onStackAction: ActionWithOrigin,
): {
  transformed: ActionWithOrigin,
  actions: ActionWithOrigin[],
  statusLog?: StatusLog,
} {
  const cond = statusToCondition(status);
  const { condition, bindings } = resolveCondition(status, cond, onStackAction);
  if (condition) {
    const st = statusToTransform(status);
    const transformed = ST.resolveStatusTransform(st.transform, bindings, status, onStackAction);
    const actions = st.actions.map(x => ST.resolveStatusTransform(x, bindings, status, onStackAction));
    const statusLog = {
      tag: status.tag,
      before: onStackAction,
      after: transformed,
    };
    return { transformed, actions, statusLog };
  } else {
    return { transformed: onStackAction, actions: [] }
  }
}