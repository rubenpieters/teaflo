
import { HasId } from "./hasId";
import { Unit, FrUnit, EnUnit } from "./unit";
import { HasAbilities } from "./ability";
import { HasAI } from "./ai";
import { frUnitMap } from "../data/units/friendly";
import { enUnitMap } from "../data/units/enemy";
import { HasThreatMap } from "./threat";
import { TargetType } from "./entityId";

export type FrStUnit = FrUnit & HasId & { cardId: string } & HasThreatMap;
export type EnStUnit = EnUnit & HasId & { cardId: string } & HasAI

export type GameState = {
  frUnits: (FrStUnit | undefined)[],
  enUnits: (EnStUnit | undefined)[],
  nextId: number,
}

export function mkGameState(
  frUnits: (string | undefined)[],
  enUnits: (string | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (FrStUnit | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId = i;
      return {...frUnitMap[x], ...{ id: i, cardId: x, threatMap: {} }}
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (EnStUnit | undefined)[] = enUnits.map((x, i) => {
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

type UnitsResult = {
  "friendly": (FrStUnit | undefined)[],
  "enemy": (EnStUnit | undefined)[],
}

export function units<A extends TargetType>(
  state: GameState,
  type: A
): UnitsResult[A] {
  switch (type) {
    case "friendly": {
      return state.frUnits;
    }
    case "enemy": {
      return state.enUnits;
    }
  }
  throw "impossible";
}