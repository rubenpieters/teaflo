import { focus, over, set } from "../iassign-util";
import { HKT, URIS, Type } from "fp-ts/lib/HKT";
import { UnitId, TargetId, EnemyId, FriendlyId } from "./entityId";
import { GameState, addThreat } from "./state";
import { damageEntity } from "./entity";
import { useChargeUnit, moveAIUnit } from "./unit";
import { AIDirection } from "./ai";
import { Status } from "./status";

/**
 * HKT boilerplate
 */
declare module "fp-ts/lib/HKT" {
  interface URI2HKT<A> {
    Action: A,
  }
}

export type ACTION_URIS = Ability_URI | Action_URI | Target_URI;

export const Ability_URI: "Ability" = "Ability";
export type Ability_URI = typeof Ability_URI;

export const Action_URI: "Action" = "Action";
export type Action_URI = typeof Action_URI;

export const Target_URI: "Target" = "Target";
export type Target_URI = typeof Target_URI;

export const Condition_URI: "Cond" = "Cond";
export type Condition_URI = typeof Condition_URI;

export const StatusTransform_URI: "ST" = "ST";
export type StatusTransform_URI = typeof StatusTransform_URI;

/**
 * A generic shape of actions.
 */
export class Damage<F extends URIS, G extends URIS> {
  public readonly tag: "Damage" = "Damage";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    readonly value: Type<F, number>,
    readonly target: Type<G, TargetId>,
  ) {}
}

export class UseCharge<F extends URIS, G extends URIS> {
  public readonly tag: "UseCharge" = "UseCharge";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    readonly value: Type<F, number>,
    readonly target: Type<G, UnitId>,
  ) {}
}

export class Invalid {
  public readonly tag: "Invalid" = "Invalid";

  constructor(
  ) {}
}

export class Death<G extends URIS> {
  public readonly tag: "Death" = "Death";

  constructor(
    readonly uriG: G,
    readonly target: Type<G, UnitId>,
  ) {}
}

export class Combined<F extends URIS, G extends URIS> {
  public readonly tag: "Combined" = "Combined";

  constructor(
    public readonly list: ActionF<F, G>[],
  ) {}
}

export class MoveAI<F extends URIS, G extends URIS> {
  public readonly tag: "MoveAI" = "MoveAI";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly dir: Type<F, AIDirection>,
    public readonly target: Type<G, EnemyId>,
  ) {}
}

export class AddThreat<F extends URIS, G extends URIS> {
  public readonly tag: "AddThreat" = "AddThreat";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly value: Type<F, number>,
    public readonly forAlly: Type<G, FriendlyId>,
    public readonly atEnemy: Type<G, EnemyId>,
  ) {}
}

export class AddStatus<F extends URIS, G extends URIS> {
  public readonly tag: "AddStatus" = "AddStatus";

  constructor(
    readonly uriF: F,
    readonly uriG: G,
    public readonly status: Type<F, Status>,
    public readonly target: Type<G, UnitId>,
  ) {}
}

export function combinedAction(
  list: ActionF<"Action", "Action">[],
): Combined<"Action", "Action"> {
  return new Combined(list);
}

export function combinedAbility(
  list: ActionF<"Ability", "Target">[],
): Combined<"Ability", "Target"> {
  return new Combined(list);
}

export type ActionF<F extends URIS, G extends URIS>
  = Damage<F, G>
  | UseCharge<F, G>
  | Invalid
  | Death<G>
  | Combined<F, G>
  | MoveAI<F, G>
  | AddThreat<F, G>
  | AddStatus<F, G>
  ;

export const actionTags: Action["tag"][]
  = [ "Damage",
      "UseCharge",
    ]
  ;

/**
 * An Action describes a single step in gamestate transitions.
 */
export type Action
  = ActionF<Action_URI, Action_URI>
  ;

export type StActionF<F extends URIS>
  = ActionF<F, F>
  & { origin: Type<F, UnitId> }
  ;

export function resolveAction(
  state: GameState,
  action: Action,
): { state: GameState, actions: Action[] } {
  switch (action.tag) {
    case "Damage": {
      const result = state.overTarget(
        action.target,
        x => damageEntity(x, action.value),
      );

      const entity = result.entity;
      let actions: Action[] = [];
      if (entity !== undefined && entity.hp <= 0) {
        actions = [new Invalid()];
      }
      return { state: result.state, actions };
    }
    case "UseCharge": {
      const result = state.overTarget(
        action.target,
        x => useChargeUnit(x, action.value),
      );

      const entity = result.entity;
      let actions: Action[] = [];
      if (entity !== undefined && entity.charges <= 0) {
        actions = [new Invalid()];
      }
      return { state: result.state, actions };
    }
    case "AddThreat": {
      const result = state.overTarget(
        action.forAlly,
        x => addThreat(x, action.atEnemy, action.value),
      );
      return { state: result.state, actions: [] };
    }
    case "AddStatus": {
      const result = focus(state,
        over(x => x.statusRow, x => x.addStatus(action.status, action.target, state.nextId)),
        over(x => x.nextId, x => x + 1),
      );
      return { state: result, actions: [] };
    }
    case "MoveAI": {
      const result = state.overTarget(
        action.target,
        x => moveAIUnit(x, action.dir),
      );
      return { state: result.state, actions: [] };
    }
    case "Invalid": {
      throw "unimplemented";
    }
    case "Death": {
      const result = state.removeTarget(action.target);
      const entity = result.entity;
      
      let actions: Action[] = [];
      if (entity !== undefined && entity.essential) {
        actions = [new Invalid()];
      }

      return { state: result.state, actions };
    }
    case "Combined": {
     return { state, actions: action.list };
    }
  }
}

export function hoistActionF<F extends URIS, G extends URIS, H extends URIS, I extends URIS>(
  actionF: ActionF<F, G>,
  newUriF: H,
  newUriG: I,
  f: <A>(fa: Type<F, A>) => Type<H, A>,
  g: <A>(fa: Type<G, A>) => Type<I, A>,
): ActionF<H, I> {
  switch (actionF.tag) {
    case "Damage": {
      const newValue = f(actionF.value);
      const newTarget = g(actionF.target);
      return new Damage(newUriF, newUriG, newValue, newTarget);
    }
    case "UseCharge": {
      const newValue = f(actionF.value);
      const newTarget = g(actionF.target);
      return new UseCharge(newUriF, newUriG, newValue, newTarget);
    }
    case "AddThreat": {
      const newValue = f(actionF.value);
      const newForAlly = g(actionF.forAlly);
      const newAtEnemy = g(actionF.atEnemy);
      return new AddThreat(newUriF, newUriG, newValue, newForAlly, newAtEnemy);
    }
    case "AddStatus": {
      const newValue = f(actionF.status);
      const newTarget = g(actionF.target);
      return new AddStatus(newUriF, newUriG, newValue, newTarget);
    }
    case "MoveAI": {
      const newDir = f(actionF.dir);
      const newTarget = g(actionF.target);
      return new MoveAI(newUriF, newUriG, newDir, newTarget);
    }
    case "Invalid": return actionF;
    case "Death": {
      const newTarget = g(actionF.target);
      return new Death(newUriG, newTarget);
    }
    case "Combined": {
      const l = actionF.list.map(x => hoistActionF(x, newUriF, newUriG, f, g));
      return new Combined(l);
    }
  }
}