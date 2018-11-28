import { enUnitMap, frUnitMap } from "../data/units/units";
import { HasId } from "./hasId";
import { Unit, FrUnit, EnUnit } from "./unit";
import { HasAbilities } from "./ability";
import { HasAI } from "./ai";

export type GameState = {
  frUnits: (Unit & HasId & { cardId: string } & HasAbilities | undefined)[],
  enUnits: (Unit & HasId & { cardId: string } & HasAI | undefined)[],
  nextId: number,
}

export function mkGameState(
  frUnits: (string | undefined)[],
  enUnits: (string | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (FrUnit & HasId & { cardId: string } | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId = i;
      return {...frUnitMap[x], ...{ id: i, cardId: x }}
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (EnUnit & HasId & { cardId: string } | undefined)[] = enUnits.map((x, i) => {
    if (x !== undefined) {
      enLastId = i;
      return {...enUnitMap[x], ...{ id: i + frLastId, cardId: x }}
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