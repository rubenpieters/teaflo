import { Unit } from "src/shared/game/unit";
import { mkDamage } from "src/shared/game/action";

export const unit1 = {
  hp: 10,
  maxHp: 10,
  charges: 5,
  maxCharges: 5,
  abilities: [
    mkDamage({ tag: "PositionId", type: "enemy", id: 0 }, 1),
  ]
}

export const unitMap: {
  [key: string]: Unit,
} = {
  "card1": unit1,
  "card2": unit1,
  "card3": unit1,
}