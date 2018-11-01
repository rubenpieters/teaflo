import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Crew } from "src/shared/game/crew";
import * as allAbilities from "src/shared/data/ability";
import * as allTriggers from "src/shared/data/trigger";
import * as allInstances from "src/shared/data/instance";
import { evStatic, evAnd, evAllies, evSelf, damage, addTarget, queueStatus, noTarget, chargeUse, heal, noop, evCondition, evTrigger, extra, addThreat, evEnemies, setHP, loseFragments, hasBubble, addInstance, leech, explode } from "src/shared/game/effectvar";
import { Poison, Guard, Bubble, DmgBarrier, Convert, Mark } from "src/shared/game/status";

export const basicDmg01: Crew = {
  ap: 1,
  hp: 45,
  maxHp: 45,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => damage(target0, evStatic(10), evStatic(false))),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const basicTank01: Crew = {
  ap: 1,
  hp: 70,
  maxHp: 70,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(evEnemies(enemy => addThreat(evSelf, evStatic(15), enemy))),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const basicUtil01: Crew = {
  ap: 1,
  hp: 35,
  maxHp: 35,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => queueStatus(target0, evStatic(<Bubble>{
      tag: "Bubble",
      value: 1,
      fragment: 0,
    }))),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const dmgPoison: Crew = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => evAnd(
      damage(target0, evStatic(15), evStatic(false)),
      queueStatus(target0, evStatic(<Poison>{
        tag: "Poison",
        value: 0,
        fragment: 50,
      })),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const tank_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(evAnd(
      evAllies(ally => damage(ally, evStatic(5), evStatic(false))),
      queueStatus(evSelf, evStatic(<Bubble>{
        tag: "Bubble",
        value: 1,
        fragment: 0,
      })),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [
    {
      tag: "Intercept",
      fragment: 0,
      value: 1,
    },
  ],
  transforms: [],
};

export const tank_02: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(evAnd(
      queueStatus(evSelf, evStatic(<Guard>{
        tag: "Guard",
        value: 1,
        guard: 10,
        fragment: 0,
      })),
      evEnemies(enemy => addThreat(evSelf, evStatic(10), enemy)),
    )),
    noTarget(
      queueStatus(evSelf, evStatic(<DmgBarrier>{
        tag: "DmgBarrier",
        damage: 3,
        value: 5,
        fragment: 0,
      }))
    ),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const tank_03: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(
      evAllies(ally => evAnd(
        setHP(ally, evStatic(1)),
        queueStatus(ally, evStatic(<Bubble>{
          tag: "Bubble",
          value: 1,
          fragment: 0,
        })),
      )),
    ),
    addTarget(0, target0 => evAnd(
      hasBubble(target0,
        evAnd(
          loseFragments(target0, evStatic(<"Bubble">"Bubble"), evStatic(100)),
          evEnemies(enemy => addThreat(evSelf, evStatic(10), enemy)),
        ),
        noop(),
      )
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const dmg_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => evAnd(
      chargeUse(evSelf, evStatic(4)),
      damage(target0, evStatic(20), evStatic(false)),
    )),
    noTarget(
      evEnemies(enemy => damage(enemy, evStatic(5), evStatic(false)))
    ),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const dmg_02: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(addInstance(
      evStatic(allInstances.instance_01),
      evStatic(<"ally">"ally"),
    )),
    noTarget(addInstance(
      evStatic(allInstances.instance_02),
      evStatic(<"ally">"ally"),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const dmg_03: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(
      evAllies(ally => queueStatus(ally, evStatic(<Convert>{
        tag: "Convert",
        value: 1,
        fragment: 0,
      })))
    ),
    addTarget(0, target0 =>
      explode(target0, evStatic(0), evStatic(3)),
    ),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const dmg_04: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => evAnd(
      queueStatus(target0, evStatic(<Mark>{
        tag: "Mark",
        value: 0,
        fragment: 50,
      })),
    )),
    addTarget(0, target0 => leech(target0)),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};

export const util_01: Crew = {
  ap: 1,
  hp: 60,
  maxHp: 60,
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    noTarget(queueStatus(evSelf, evStatic(<Guard>{
      tag: "Guard",
      value: 2,
      guard: 10,
      fragment: 0,
    }))),
    noTarget(evAnd(
      evAllies(ally => heal(ally, evStatic(30))),
      damage(evSelf, evStatic(40), evStatic(false)),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
  transforms: [],
};