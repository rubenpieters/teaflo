import { focus, over, set } from "src/shared/iassign-util";
import { Crew } from "src/shared/game/crew";

type Damage = {
  tag: "Damage",
  positions: number[],
  value: number,
}

type Gain = {
  tag: "Gain",
}

export type Effect
  = Damage
  | Gain

export function applyEffect(
  effect: Effect,
  crew: Crew[],
): Crew[] | "invalid" {
  /* crew interactions with effects
  for (const ally of crew) {

  }
  */

  switch (effect.tag) {
    case "Gain": {
      return crew;
    }
    case "Damage": {
      let resultCrew: Crew[] = crew;
      for (const position in effect.positions) {
        const allyAtPos: Crew | undefined = crew[position];
        if (allyAtPos === undefined) {
          return "invalid";
        }
        resultCrew = focus(resultCrew,
          over(x => x[position].hp, x => x - effect.value)
        )
      }
      return resultCrew;
    }
  }
}