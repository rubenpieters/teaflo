import { EntityEffect } from "src/shared/game/ability";

export type Instance = {
  ap: number,
  hp: number,
  maxHp: number,
  ranged: boolean,
  actions: EntityEffect[],
  // triggers: TriggerEntityEffect[],
};