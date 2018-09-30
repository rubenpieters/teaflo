import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Crew } from "src/shared/game/crew";
import * as allAbilities from "src/shared/data/ability";
import * as allTriggers from "src/shared/data/trigger";
import { damageI, evInput, evStatic, evAnd, queueStatusI, withI, evAllAlly, evSelf, chargeUseI, healI } from "../game/effectvar";
import { Poison, Guard, Bubble } from "../game/status";

export const armorOnSelfHeal: Crew = {
  ap: 1,
  hp: 5,
  maxHp: 5,
  triggers: [
    allTriggers.armorOnHeal,
    allTriggers.poisonToPiercing,
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    allAbilities.armorDamageToTarget,
  ],
  threatMap: {},
  charges: 5,
};


export const regenOnDamageAlly: Crew = {
  ap: 1,
  hp: 6,
  maxHp: 6,
  triggers: [
    allTriggers.regenOnDamage,
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    allAbilities.armorAllAlly_5_1_0,
  ],
  threatMap: {},
  charges: 5,
};

export function onAllAlly(
  state: GameState,
  f: (ally: IdCrew, id: number) => Action,
): Action {
  const actions = state.crew.map(f);
  return {
    tag: "CombinedAction",
    actions,
  }
}


export const tank1: Crew = {
  ap: 1,
  hp: 20,
  maxHp: 20,
  triggers: [
    allTriggers.interceptAllyDamage,
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
  ],
  threatMap: {},
  charges: 5,
};

export const dmg1: Crew = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  triggers: [
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(damageI(evInput(0), evStatic(15), evStatic(false)), { tag: "NumberInput" }),
    withI(damageI(evInput(0), evStatic(10), evStatic(false)), { tag: "NumberInput" }),
  ],
  threatMap: {},
  charges: 5,
};

export const dmgPoison: Crew = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  triggers: [
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(evAnd(
      damageI(evInput(0), evStatic(15), evStatic(false)),
      queueStatusI(evInput(0), evStatic(<Poison>{
        tag: "Poison",
        value: 0,
        fragment: 50,
      })),
    ),
    { tag: "NumberInput" }),
  ],
  threatMap: {},
  charges: 5,
};

export const basicCrew1: Crew = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  triggers: [
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(queueStatusI(evInput(0), evStatic(<Guard>{
      tag: "Guard",
      value: 1,
      guard: 8,
      fragment: 0,
    }))),
    withI(damageI(evInput(0), evStatic(15), evStatic(false)), { tag: "NumberInput" }),
  ],
  threatMap: {},
  charges: 5,
};

export const tank_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
    allTriggers.addThreatOnDamage,
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(evAnd(
      evAllAlly((ally) => damageI(ally, evStatic(5), evStatic(false))),
      queueStatusI(evSelf, evStatic(<Bubble>{
        tag: "Bubble",
        value: 1,
        fragment: 0,
      })),
    )),
  ],
  threatMap: {},
  charges: 5,
};

export const dmg_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(evAnd(
      chargeUseI(evSelf, evStatic(4)),
      damageI(evInput(0), evStatic(20), evStatic(false)),
    ),
    { tag: "NumberInput" }),
  ],
  threatMap: {},
  charges: 5,
};

export const util_01: Crew = {
  ap: 1,
  hp: 60,
  maxHp: 60,
  triggers: [
  ],
  ranged: false,
  actions: [
    allAbilities.noop,
  ],
  abilities: [
    withI(queueStatusI(evSelf, evStatic(<Guard>{
      tag: "Guard",
      value: 2,
      guard: 10,
      fragment: 0,
    }))),
    withI(evAnd(
      damageI(evSelf, evStatic(20), evStatic(false)),
      evAllAlly((ally) => healI(ally, evStatic(30))),
    )),
  ],
  threatMap: {},
  charges: 5,
};