import { noTarget, damage, evStatic, queueStatus } from "src/shared/game/effectvar";
import { CreatureId } from "src/shared/game/state";
import { Instance } from "src/shared/game/instance";
import { Weak } from "src/shared/game/status";

export const instance_01: Instance = {
  ap: 1,
  hp: 30,
  maxHp: 30,
  action: damage(
    evStatic(<CreatureId>{ tag: "PositionId", type: "enemy", id: 0 }), evStatic(1), evStatic(false))
}

export const instance_02: Instance = {
  ap: 1,
  hp: 30,
  maxHp: 30,
  action: queueStatus(
    evStatic(<CreatureId>{ tag: "PositionId", type: "enemy", id: 0 }),
    evStatic(<Weak>{
      tag: "Weak",
      value: 0,
      fragment: 25,
    })
  )
}