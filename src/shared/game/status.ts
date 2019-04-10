import { Damage, Death, Action } from "./action";
import * as C from "./condition";
import * as ST from "./statusTranform";
import { GameState } from "./state";
import { StatusLog } from "./log";
import { StStatus } from "./statusRow";

/**
 * A status is a lingering effect on the gamestate.
 */
export type Status
  = Weak
  | Strong
  | Armor
  | Fragile
  ;

export class Weak {
  public readonly tag: "Weak" = "Weak";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Strong {
  public readonly tag: "Strong" = "Strong";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Armor {
  public readonly tag: "Armor" = "Armor";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

export class Fragile {
  public readonly tag: "Fragile" = "Fragile";
  public readonly hp: number;
  public readonly maxHp: number;

  constructor(
    public readonly value: number,
  ) {
    this.maxHp = statusModifier(this.tag) * value;
    this.hp = this.maxHp;
  }
}

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

export type StatusGroup
  = "atk_mod"
  | "def_mod"
  ;

export const groupOrder: StatusGroup[]
  = ["atk_mod", "def_mod"];

export function statusModifier(
  statusTag: StatusTag,
): number {
  switch (statusTag) {
    case "Armor": return 1;
    case "Fragile": return 1;
    case "Strong": return 7;
    case "Weak": return 7;
  }
}

export type StatusTag = Status["tag"];

export const statusTags: StatusTag[]
  = ["Weak", "Strong", "Armor", "Fragile"];

export function statusToCondition(
  status: Status,
): C.ActionCondition {
  switch (status.tag) {
    case "Fragile":
    case "Armor": {
      return new Damage("Cond", "Cond", new C.Var("1"), C.statusOwner());
    }
    default: {
      throw "unimpl";
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
      // we actually want to keep all properties, except the damage value
      const transform = new Damage("ST", "ST", ST.monus(new C.Var("1"), C.statusValue()), C.statusOwner());
      const actions: ST.StatusTransform[] = [
        //new Death("Target", id of the armor status);
      ];
      return { transform, actions };
    }
    default: {
      throw "unimpl";
    }
  }
}

export function applyStatuses(
  onStackAction: Action,
  state: GameState,
) {
  let newActions: Action[] = [];
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
  onStackAction: Action,
): {
  transformed: Action,
  actions: Action[],
  statusLog?: StatusLog,
} {
  const cond = statusToCondition(status);
  const { condition, bindings } = C.resolveCondition(status, cond, onStackAction);
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