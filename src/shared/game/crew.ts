import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew, CreatureId, toGlobalId } from "src/shared/game/state";
import { Action, ActionSpec, applyActionAndTriggers } from "src/shared/game/action";
import { Generator } from "src/shared/handler/id/generator";
import { HasStatus, Guard, Status, StatusTag, TransformTag } from "src/shared/game/status";
import { InputType } from "src/shared/game/input";
import { InputEntityEffect, EntityEffect, TriggerEntityEffect } from "src/shared/game/ability";
import { Transform } from "src/shared/game/status";
import { ApplyActionLog } from "./log";

/*export function showCrew(
  crew: Crew
) {
  return {...crew,
    actions: crew.actions.map(showAction),
    triggers: crew.triggers.map(showTrigger) };
}*/

type ThreatMap = {[globalId: number]: number};

export type Ability = {
  f: (inputs: any[]) => ActionSpec,
  inputs: InputType[],
};

export type Crew = {
  ap: number,
  hp: number,
  maxHp: number,
  ranged: boolean,
  actions: EntityEffect[],
  abilities: InputEntityEffect[],
  threatMap: ThreatMap,
  charges: number,
  fragmentLoss: { [key in StatusTag | TransformTag]?: number },
  status: Status[],
  transforms: Transform[],
};

export function damage<E extends Crew & HasStatus>(
  crew: E,
  damage: number,
  piercing: boolean,
): E {
  return focus(crew, over(x => x.hp, x => x - damage));
  /*if (piercing || (crew.status.Guard === undefined && crew.status.Bubble === undefined)) {
    return focus(crew, over(x => x.hp, x => x - damage));
  } else {
    if (crew.status.Bubble !== undefined) {
      return focus(crew,
        set(x => x.status.Bubble, undefined),
      );
    } else { // crew.Guard !== undefined
      if (damage <= crew.status.Guard!.guard) {
        return focus(crew, over(x => (<Guard>x.status.Guard).guard, x => x - damage));
      }
  
      const leftoverDamage = damage - crew.status.Guard!.guard;
      return focus(crew,
        set(x => x.status.Guard, undefined),
        over(x => x.hp, x => x - leftoverDamage),
      );
    }
  }*/
}

export function addThreat<E extends Crew>(
  state: GameState,
  crew: E,
  threat: number,
  enemyId: CreatureId,
): E {
  const enemyGid = toGlobalId(state, enemyId).id;
  if (crew.threatMap[enemyGid] !== undefined) {
    return focus(crew,
      over(x => x.threatMap[enemyGid], x => x + threat),
    );
  } else {
    return focus(crew,
      set(x => x.threatMap[enemyGid], threat),
    );
  }
}

export function useCharge<E extends { charges: number }>(
  crew: E,
  chargeUse: number,
): E {
  // assumption: chargeUse < crew.charges
  return focus(crew,
    over(x => x.charges, x => x - chargeUse),
  );
}

export function setHP<E extends { hp: number }>(
  e: E,
  value: number,
): E {
  // TODO: what to do when hp > max?
  return focus(e,
    set(x => x.hp, value),
  );
}

export function heal<E extends Crew>(
  crew: E,
  amount: number,
) {
  if (crew.hp + amount > crew.maxHp) {
    return focus(crew, set(x => x.hp, crew.maxHp));
  }
  return focus(crew, set(x => x.hp, crew.hp + amount));
}

export function addHP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew,
    over(x => x.hp, x => x + amount),
    over(x => x.maxHp, x => x + amount),
  );
}

export function addAP<E extends Crew>(
  crew: E,
  amount: number
) {
  return focus(crew, over(x => x.ap, x => x + amount));
}

export function damageAP<E extends Crew>(
  crew: E,
  amount: number
) {
  if (amount >= crew.ap) {
    return focus(crew, set(x => x.ap, 0));
  }
  return focus(crew, over(x => x.ap, x => x - amount));
}

export function getAP<C extends Crew>(
  crew: C,
  multiplier: number,
): number {
  return multiplier * (crew.ap + 0); // TODO: add status for increasing damage
}

export function act(
  crew: IdCrew,
  state: GameState,
  log: ApplyActionLog,
  idGen: Generator,
  index: number,
): { state: GameState | "invalid", log: ApplyActionLog }  {
  const action: EntityEffect = crew.actions[crew.actionIndex];
  state = focus(state,
    over(x => x.crew[index].actionIndex, x => {
      const newX = x + 1;
      return newX >= crew.actions.length ? 0 : newX;
    }),
  );
  return applyActionAndTriggers(
    action.effect({ state, selfId: { tag: "GlobalId", id: crew.id, type: "ally" }}).action,
    state, log, idGen, { tag: "GlobalId", id: crew.id, type: "ally" }
  );
}