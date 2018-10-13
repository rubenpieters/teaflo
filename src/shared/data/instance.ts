import { noTarget, damage, evStatic } from "../game/effectvar";
import { CreatureId } from "../game/state";
import { Instance } from "../game/instance";


export const instance_01: Instance = {
  ap: 1,
  hp: 30,
  maxHp: 30,
  actions: [
    damage(evStatic(<CreatureId>{ tag: "PositionId", type: "enemy", id: 0}), evStatic(1), evStatic(false)),
  ],
}