import { EntityEffect } from "src/shared/game/ability";

export type Instance = {
  ap: number,
  hp: number,
  maxHp: number,
  action: EntityEffect,
  // triggers: TriggerEntityEffect[],
};