import { focus, over, set } from "../iassign-util";
import { URIS, Type } from "fp-ts/lib/HKT";
import { overTarget, removeTarget } from "./state";
import { damageEntity, healEntity } from "./entity";
import { useChargeUnit, moveAIUnit, restoreChargeUnit } from "./unit";
import { statusGroup, statusDescription } from "./status";
import { addThreat, removeThreat } from "./threat";
import { Action, ActionWithOrigin, ActionWithOriginF } from "../definitions/action";
import { GameState, StFrUnit } from "../definitions/state";
import { invalidNoOrigin, ActionF, Damage, UseCharge, AddThreat, AddStatus, MoveAI, Death, Combined, ActionTag, RestoreCharge, Heal, RemoveThreat } from "../definitions/actionf";
import { addStatus } from "./statusRow";
import { descSingleton, numberDescription } from "./description";
import { DescToken, DescSymbol } from "../definitions/description";
import { HasId } from "../definitions/entityId";
import { routeDirectionDescription } from "./ai";



export const actionTags: Action["tag"][]
  = [ "Damage",
      "UseCharge",
    ]
  ;

export function resolveAction(
  state: GameState,
  action: ActionWithOrigin,
): { state: GameState, actions: ActionWithOrigin[] } {
  switch (action.tag) {
    case "Damage": {
      const result = overTarget(state,
        action.target,
        x => damageEntity(x, action.value),
      );

      const entity = result.entity;
      let actions: ActionWithOrigin[] = [];
      if (
        entity !== undefined &&
        entity.hp <= 0
      ) {
        if (
          (entity as HasId<"friendly">).id.type === "friendly" &&
          (entity as StFrUnit).essential
        ) {
          actions = [invalidNoOrigin];
        } else {
          actions = [{
            ...new Death("Action", entity.id),
            origin: "noOrigin",
          }];
        }
      }
      return { state: result.state, actions };
    }
    case "Heal": {
      const result = overTarget(state,
        action.target,
        x => healEntity(x, action.value),
      );

      const actions: ActionWithOrigin[] = [];

      return { state: result.state, actions };
    }
    case "UseCharge": {
      const result = overTarget(state,
        action.target,
        x => useChargeUnit(x, action.value),
      );

      const entity = result.entity;
      let actions: ActionWithOrigin[] = [];
      if (entity !== undefined && entity.charges < 0) {
        actions = [invalidNoOrigin];
      }
      return { state: result.state, actions };
    }
    case "RestoreCharge": {
      const result = overTarget(state,
        action.target,
        x => restoreChargeUnit(x, action.value),
      );

      const actions: ActionWithOrigin[] = [];

      return { state: result.state, actions };
    }
    case "AddThreat": {
      const result = overTarget(state,
        action.forAlly,
        x => addThreat(x, action.atEnemy, action.value),
      );
      return { state: result.state, actions: [] };
    }
    case "RemoveThreat": {
      const result = overTarget(state,
        action.forAlly,
        x => removeThreat(x, action.atEnemy, action.value),
      );
      return { state: result.state, actions: [] };
    }
    case "AddStatus": {
      const group = statusGroup(action.status);
      const result = focus(state,
        over(x => x.statusRows[group], x => addStatus(x, action.status, action.target, state.nextId)),
        over(x => x.nextId, x => x + 1),
      );
      return { state: result, actions: [] };
    }
    case "MoveAI": {
      const result = overTarget(state,
        action.target,
        x => moveAIUnit(x, action.dir),
      );
      return { state: result.state, actions: [] };
    }
    case "Invalid": {
      const result = focus(state,
        set(x => x.type, "invalid"),
      );
      return { state: result, actions: [] };
    }
    case "Death": {
      const result = removeTarget(state, action.target);
      const entity = result.entity;
      
      let actions: ActionWithOrigin[] = [];
      if (entity !== undefined && entity.id.type === "friendly" && (entity as any).essential) {
        actions = [invalidNoOrigin];
      }

      return { state: result.state, actions };
    }
    case "Combined": {
      const actions = action.list.map(x => {
        return {...x, origin: action.origin }
      });
     return { state, actions };
    }
    case "StartTurn": {
      return { state, actions: [] };
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
    case "Heal": {
      const newValue = f(actionF.value);
      const newTarget = g(actionF.target);
      return new Heal(newUriF, newUriG, newValue, newTarget);
    }
    case "UseCharge": {
      const newValue = f(actionF.value);
      const newTarget = g(actionF.target);
      return new UseCharge(newUriF, newUriG, newValue, newTarget);
    }
    case "RestoreCharge": {
      const newValue = f(actionF.value);
      const newTarget = g(actionF.target);
      return new RestoreCharge(newUriF, newUriG, newValue, newTarget);
    }
    case "AddThreat": {
      const newValue = f(actionF.value);
      const newForAlly = g(actionF.forAlly);
      const newAtEnemy = g(actionF.atEnemy);
      return new AddThreat(newUriF, newUriG, newValue, newForAlly, newAtEnemy);
    }
    case "RemoveThreat": {
      const newValue = f(actionF.value);
      const newForAlly = g(actionF.forAlly);
      const newAtEnemy = g(actionF.atEnemy);
      return new RemoveThreat(newUriF, newUriG, newValue, newForAlly, newAtEnemy);
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
    case "StartTurn": return actionF;
  }
}

export function hoistActionWithOriginF<F extends URIS, H extends URIS>(
  actionF: ActionWithOriginF<F>,
  newUriF: H,
  f: <A>(fa: Type<F, A>) => Type<H, A>,
): ActionWithOriginF<H> {
  const newAction = hoistActionF(actionF, newUriF, newUriF, f, f);
  const newActionWithOrigin = {...newAction, origin: f(actionF.origin) };
  return newActionWithOrigin;
}

export function ignoreTag(
  actionTag: ActionTag,
) {
  return actionTag === "Combined";
}

/**
 * Description shown when an action is performed on the state.
 */
export function actionDescription(
  action: Action,
): DescToken[] {
  switch (action.tag) {
    case "AddThreat": {
      return descSingleton("icon_plus")
        .concat(numberDescription(action.value))
        .concat(new DescSymbol("icon_th"))
        ;
    }
    case "RemoveThreat": {
      return descSingleton("icon_minus")
        .concat(numberDescription(action.value))
        .concat(new DescSymbol("icon_th"))
        ;
    }
    case "AddStatus": {
      return descSingleton("icon_plus")
        .concat(statusDescription(action.status))
        ;
    }
    case "Combined": {
      throw "actionDescription: encountered a Combined action";
    }
    case "Damage": {
      return descSingleton("icon_minus")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("icon_hp"))
      ;
    }
    case "Heal": {
      return descSingleton("icon_plus")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("icon_hp"))
      ;
    }
    case "UseCharge": {
      return descSingleton("icon_minus")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("icon_ch"))
      ;
    }
    case "RestoreCharge": {
      return descSingleton("icon_plus")
      .concat(numberDescription(action.value))
      .concat(new DescSymbol("icon_ch"))
      ;
    }
    case "MoveAI": {
      return routeDirectionDescription(action.dir)
        ;
    }
    case "Death": {
      return descSingleton("icon_death")
        ;
    }
    case "StartTurn": {
      return descSingleton("icon_start_turn")
        ;
    }
    case "Invalid": {
      return descSingleton("icon_invalid")
        ;
    }
  }
}