import { focus, over, set } from "src/shared/iassign-util";

export type Crew = {
  ap: number,
  hp: number,
}

const stFighter: Crew = {
    ap: 5,
    hp: 5,
}

export const allCrew = {
  stFighter: stFighter
}