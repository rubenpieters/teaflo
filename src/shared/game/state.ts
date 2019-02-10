
import { HasId } from "./hasId";
import { Unit, FrUnit, EnUnit } from "./unit";
import { HasAbilities } from "./ability";
import { HasAI } from "./ai";
import { frUnitMap } from "../data/units/friendly";
import { enUnitMap } from "../data/units/enemy";
import { HasThreatMap } from "./threat";
import { UnitType, GlobalId } from "./entityId";
import { StTrigger, Trigger, TriggerGroup, triggerOrder } from "./trigger";

export type FrStUnit = FrUnit & HasId & { cardId: string } & HasThreatMap;
export type EnStUnit = EnUnit & HasId & { cardId: string } & HasAI

export type GameState = {
  frUnits: (FrStUnit | undefined)[],
  enUnits: (EnStUnit | undefined)[],
  nextId: number,
  state: "invalid" | "win" | "default",
  triggers: {
    [K in TriggerGroup]: StTrigger[]
  }
}

export function filteredEn(
  state: GameState,
): EnStUnit[] {
  return <EnStUnit[]>state.enUnits.filter(x => x !== undefined);
}

export function filteredFr(
  state: GameState,
): FrStUnit[] {
  return <FrStUnit[]>state.frUnits.filter(x => x !== undefined);
}

export function mkGameState(
  frUnits: (string | undefined)[],
  enUnits: (string | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (FrStUnit | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId += 1;
      return {...frUnitMap[x], ...{ id: i, cardId: x, threatMap: {} }};
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (EnStUnit | undefined)[] = enUnits.map((x, i) => {
    if (x !== undefined) {
      enLastId += 1;
      return {...enUnitMap[x], ...{ id: i + frLastId, cardId: x }};
    } else {
      return undefined;
    }
  });
  return {
    frUnits: frUnitsWithId,
    enUnits: enUnitsWithId,
    nextId: enLastId + 1,
    state: "default",
    triggers: {
      "armor": [],
      "strong": [],
      "weak": [],
      "other": [],
    }
  }
}

type UnitsResult = {
  "friendly": (FrStUnit | undefined)[],
  "enemy": (EnStUnit | undefined)[],
}

export function units<A extends UnitType>(
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

export function findStatus(
  state: GameState,
  statusId: GlobalId<"status">,
): {
  group: TriggerGroup,
  index: number,
} | undefined {
  for (const group of triggerOrder) {
    const index = state.triggers[group].findIndex(x => x.id === statusId.id);
    if (index !== -1) {
      return {
        group,
        index,
      };
    }
  }
  return undefined;
}