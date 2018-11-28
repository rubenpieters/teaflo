import { Action } from "./action";

export type HasAbilities = {
  abilities: Ability[],
}

export type Ability = Action;