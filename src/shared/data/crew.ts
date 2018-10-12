import { focus, over, set } from "src/shared/iassign-util";
import { GameState, IdCrew } from "src/shared/game/state";
import { Action } from "src/shared/game/action";
import { Crew } from "src/shared/game/crew";
import * as allAbilities from "src/shared/data/ability";
import * as allTriggers from "src/shared/data/trigger";
import { evStatic, evAnd, evAllies, evSelf, damage, addTarget, queueStatus, noTarget, chargeUse, heal, noop, evCondition, evTrigger, extra, addThreat, evEnemies, setHP } from "src/shared/game/effectvar";
import { Poison, Guard, Bubble, DmgBarrier } from "src/shared/game/status";

export const dmgPoison: Crew = {
  ap: 1,
  hp: 15,
  maxHp: 15,
  triggers: [
  ],
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
};

export const tank_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
    evTrigger(trigger => evCondition(trigger,
      x => x.tag === "Damage" && x.target.type === "ally", // TODO: and target not self?
      extra(evEnemies(enemy => addThreat(evSelf, evStatic(5), enemy)), { chargeUse: 1 }),
      extra(noop(), { chargeUse: 0 }),
      ),
      "before",
    )
  ],
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
  status: [],
};

export const tank_02: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
  ],
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
};

export const tank_03: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
  ],
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
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
};

export const dmg_01: Crew = {
  ap: 1,
  hp: 100,
  maxHp: 100,
  triggers: [
  ],
  ranged: false,
  actions: [
    noop(),
  ],
  abilities: [
    addTarget(0, target0 => evAnd(
      chargeUse(evSelf, evStatic(4)),
      damage(target0, evStatic(20), evStatic(false)),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
};

export const util_01: Crew = {
  ap: 1,
  hp: 60,
  maxHp: 60,
  triggers: [
  ],
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
      damage(evSelf, evStatic(20), evStatic(false)),
      evAllies((ally) => heal(ally, evStatic(30))),
    )),
  ],
  threatMap: {},
  charges: 5,
  fragmentLoss: {},
  status: [],
};