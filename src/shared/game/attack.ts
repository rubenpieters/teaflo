import { focus, over, set } from "src/shared/iassign-util";
import { Enemy } from "src/shared/game/enemy";
import { Crew } from "src/shared/game/crew";

export type Damage = {
  tag: "Damage",
  multiplier: number,
};

export type Status = {
  tag: "Status"
};

export type Attack
  = Damage
  | Status
  ;

export function doAttack(
 crew: Crew,
 enemy: Enemy,
): Enemy {
  const attack = crew.attack;
  switch (attack.tag) {
    case "Damage": {
      return focus(enemy, over(x => x.rank, x => x - attack.multiplier * crew.ap));
    }
    case "Status": {
      return enemy;
    }
  }
}