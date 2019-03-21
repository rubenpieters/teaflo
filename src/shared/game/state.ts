
import { HasId } from "./hasId";
import { Unit, FrUnit, EnUnit } from "./unit";
import { HasAbilities, UserInput, UnitInput, StatusInput } from "./ability";
import { HasAI } from "./ai";
import { frUnitMap, FrUnitId } from "../data/units/friendly";
import { enUnitMap, EnUnitId } from "../data/units/enemy";
import { HasThreatMap } from "./threat";
import { UnitType, GlobalId, TargetId, toPositionId } from "./entityId";
import { StTrigger, Trigger, TriggerGroup, triggerOrder, showTriggerCompact } from "./trigger";
import { SolutionData } from "./solution";

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

function showUnitCompact(
  unit: Unit & HasId | undefined,
  i: number,
) {
  if (unit === undefined) {
    return `${i} - N/A`;
  } else {
    return `${i},${unit.id} - ${unit.hp}/${unit.maxHp} - ${unit.charges}/${unit.maxCharges}`;
  }
}

export function showStateCompact(
  state: GameState,
): string {
  const fr = state.frUnits.map((x, i) => showUnitCompact(x, i)).join(" | ");
  const en = state.enUnits.map((x, i) => showUnitCompact(x, i)).join(" | ");
  const tr = triggerById(state);
  const frTr = tr.fr.map((l, i) => {
    return `${i}: ${l.map(x => showTriggerCompact(x)).join(" | ")}`;
  }).join("\n");
  const enTr = tr.en.map((l, i) => {
    return `${i}: ${l.map(x => showTriggerCompact(x)).join(" | ")}`;
  }).join("\n");

  return `state:${state.state}\n${fr}\n${en}\n${frTr}\n${enTr}`;
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
  frUnits: (FrUnitId | undefined)[],
  enUnits: (EnUnitId | undefined)[],
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

export function triggerById(
  state: GameState,
): {
  fr: StTrigger[][],
  en: StTrigger[][],
} {
  const fr: StTrigger[][] = [];
  const en: StTrigger[][] = [];
  triggerOrder.forEach(tag => {
    state.triggers[tag].forEach(trigger =>{
      const posId = toPositionId(state, trigger.owner);
      switch (posId.type) {
        case "friendly": {
          if (fr[posId.id] === undefined) {
            fr[posId.id] = [trigger];
          } else {
            fr[posId.id].push(trigger);
          }
          break;
        }
        case "enemy": {
          if (en[posId.id] === undefined) {
            en[posId.id] = [trigger];
          } else {
            en[posId.id].push(trigger);
          }
          break;
        }
      }
    });
  });
  return { fr, en };
}

export function possibleActions(
  state: GameState,
): SolutionData[] {
  const actions: SolutionData[] = [];
  state.frUnits.forEach(frUnit => {
    if (frUnit !== undefined) {
      frUnit.abilities.forEach(ability => {
        if (ability.inputs.length === 0) {
          actions.push({
            ability,
            origin: new GlobalId(frUnit.id, "friendly"),
            inputs: [],
          });
        } else {
          const inputPossibilities = ability.inputs.map(input => {
            return possibleTargets(state, input);
          });
          const inputCartesian = cartesian(inputPossibilities);
          inputCartesian.forEach(inputPossibility => {
            actions.push({
              ability,
              origin: new GlobalId(frUnit.id, "friendly"),
              inputs: inputPossibility,
            });
          });
        }
      });
    }
  });

  return actions;
}

function cartesian<A>(arg: A[][]): A[][] {
  if (arg.length === 0) {
    return [];
  };
  const r: A[][] = [];
  const max = arg.length - 1;
  function helper(arr: A[], i: number) {
      for (let j = 0, l = arg[i].length; j < l; j++) {
          const a: A[] = arr.slice(0); // clone arr
          a.push(arg[i][j]);
          if (i == max)
              r.push(a);
          else
              helper(a, i+1);
      }
  }
  helper([], 0);
  return r;
}

export function possibleTargets(
  state: GameState,
  userInput: UserInput,
): TargetId[] {
  switch (userInput.tag) {
    case "NumberInput": {
      throw "unimpl";
    }
    case "TargetInput": {
      return possibleTargets(state, new UnitInput)
        .concat(possibleTargets(state, new StatusInput));
    }
    case "UnitInput": {
      const enIds: TargetId[] = filteredEn(state).map(x => new GlobalId(x.id, "enemy"));
      const frIds: TargetId[] = filteredFr(state).map(x => new GlobalId(x.id, "friendly"));
      return enIds.concat(frIds);
    }
    case "StatusInput": {
      const statusIds: TargetId[] = [];
      triggerOrder.forEach(tag => {
        state.triggers[tag].forEach(status => {
          statusIds.push(new GlobalId(status.id, "status"));
        });
      });
      return statusIds;
    }
  }
}