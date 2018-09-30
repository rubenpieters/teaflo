import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Crew } from "src/shared/game/crew";
import * as allAbilities from "src/shared/data/ability";
import * as allTriggers from "src/shared/data/trigger";
import { damageI, evInput, evStatic } from "../game/effectvar";

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
    damageI(evInput(0), evStatic(15), evStatic(false)),
    damageI(evInput(0), evStatic(10), evStatic(false)),
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
    allAbilities.dmgPoison,
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
    allAbilities.armorSelf,
    allAbilities.dmg15,
  ],
  threatMap: {},
  charges: 5,
};

export const basicCrew2: Crew = {
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
    allAbilities.addAp,
  ],
  threatMap: {},
  charges: 5,
};

export const basicCrew3: Crew = {
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
    allAbilities.apDmg,
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
    allAbilities.dmgAllGainBubble,
  ],
  threatMap: {},
  charges: 5,
};

export const dmg_01: Crew = {
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
    allAbilities.dmgHighCd,
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
    allAbilities.dmgSelfHealAllies,
    allAbilities.armorSelf10_2,
  ],
  threatMap: {},
  charges: 5,
};