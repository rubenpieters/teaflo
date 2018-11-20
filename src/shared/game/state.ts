import { unitMap } from "../data/units/units";
import { HasId } from "./hasId";
import { Unit } from "./unit";

export type GameState = {
  frUnits: (Unit & HasId | undefined)[],
  enUnits: (Unit & HasId | undefined)[],
  nextId: number,
}

export function mkGameState(
  frUnits: (string | undefined)[],
  enUnits: (string | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (Unit & HasId | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId = i;
      return {...unitMap[x], ...{ id: i }}
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (Unit & HasId | undefined)[] = enUnits.map((x, i) => {
    if (x !== undefined) {
      enLastId = i;
      return {...unitMap[x], ...{ id: i + frLastId }}
    } else {
      return undefined;
    }
  });
  return {
    frUnits: frUnitsWithId,
    enUnits: enUnitsWithId,
    nextId: enLastId,
  }
}