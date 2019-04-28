import { TargetType, EntityId, FriendlyId, EnemyId, StatusId, UnitType, friendlyId, enemyId, TargetId } from "../definitions/entityId";
import { UnitRow } from "../definitions/unitRow";
import { StatusRow, StStatus } from "../definitions/statusRow";
import { focus, over, modifyAndGet } from "../iassign-util";
import { groupOrder, showStatus } from "./status";
import deepEqual from "deep-equal";
import { frUnitMap, FrUnitId } from "../data/frUnitMap";
import { enUnitMap, EnUnitId } from "../data/enUnitMap";
import { StFrUnit, StEnUnit, GameState } from "../definitions/state";
import { defined, overUnit, removeUnit } from "./unitRow";
import { statusPosition, overStatus, removeStatus } from "./statusRow";
import { UserInput, UnitInput, StatusInput, FriendlyInput, EnemyInput } from "../definitions/input";
import { SolutionData } from "./solution";
import { showUnit } from "./unit";

export type IdToEntityType = {
  "friendly": StFrUnit,
  "enemy": StEnUnit,
  "status": StStatus,
}

export function mkGameState(
  frUnits: (FrUnitId | undefined)[],
  enUnits: (EnUnitId | undefined)[],
): GameState {
  let frLastId = 0;
  const frUnitsWithId: (StFrUnit | undefined)[] = frUnits.map((x, i) => {
    if (x !== undefined) {
      frLastId += 1;
      return {...frUnitMap[x], id: friendlyId(i), cardId: x, threatMap: {}, };
    } else {
      return undefined;
    }
  });
  let enLastId = 0;
  const enUnitsWithId: (StEnUnit | undefined)[] = enUnits.map((x, i) => {
    if (x !== undefined) {
      enLastId += 1;
      return {...enUnitMap[x], id: enemyId(i + frLastId), };
    } else {
      return undefined;
    }
  });
  const statusRows = {} as any;
  groupOrder.forEach(group => {
    statusRows[group] = new StatusRow();
  });
  return new GameState(
    new UnitRow("friendly", frUnitsWithId),
    new UnitRow("enemy", enUnitsWithId),
    enLastId + 1,
    statusRows
  );
}

export function frFiltered(
  state: GameState
): { e: StFrUnit, i: number }[] {
  return defined(state.frUnits);
}

export function frIds(
  state: GameState,
): { e: FriendlyId, i: number }[] {
  return frFiltered(state).map(x => {
    return { e: x.e.id, i: x.i };
  });
}

export function enFiltered(
  state: GameState,
): { e: StEnUnit, i: number }[] {
  return defined(state.enUnits);
}

export function enIds(
  state: GameState,
): { e: EnemyId, i: number }[] {
  return enFiltered(state).map(x => {
    return { e: x.e.id, i: x.i };
  });
}

export function position<Type extends UnitType>(
  state: GameState,
  _target: EntityId<Type>,
): number | undefined {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;

      const index = state.frUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
      if (index === -1) return undefined;
      return index;
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;

      const index = state.enUnits.units.findIndex(x => x !== undefined && deepEqual(x.id, target));
      if (index === -1) return undefined;
      return index;
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}

export function stStatusPosition(
  state: GameState,
  statusId: StatusId,
): { rowPosition: number, columnPosition: number } | undefined {
  let rowPosition = 0;
  for (const group of groupOrder) {
    const columnPosition = statusPosition(state.statusRows[group], statusId);
    if (columnPosition !== undefined) {
      return { rowPosition, columnPosition };
    }
    rowPosition += 1;
  }
  return undefined;
}

export function overTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
  _f: (e: IdToEntityType[Type]) => IdToEntityType[Type],
): { state: GameState, entity?: IdToEntityType[Type] } {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;
      const f = _f as (e: StFrUnit) => StFrUnit;

      const result = modifyAndGet(state,
        x => x.frUnits, x => {
          const result = overUnit(x, target, f);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;
      const f = _f as (e: StEnUnit) => StEnUnit;

      const result = modifyAndGet(state,
        x => x.enUnits, x => {
          const result = overUnit(x, target, f);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "status": {
      // compiler does not refine `Type`
      const target = _target as StatusId;
      const f = _f as (e: StStatus) => StStatus;

      for (const group of groupOrder) {
        const result = modifyAndGet(state,
          x => x.statusRows[group], x => {
            const result = overStatus(x, target, f);
            return { a: result.row, b: result.status };
          }
        );
        if (result.b !== undefined) {
          return { state: result.s, entity: result.b };
        }
      }

      return { state };
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}

export function getTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
): IdToEntityType[Type] | undefined {
  return overTarget(state, _target, x => x).entity;
}

export function removeTarget<Type extends TargetType>(
  state: GameState,
  _target: EntityId<Type>,
): { state: GameState, entity?: IdToEntityType[Type] } {
  switch (_target.type) {
    case "friendly": {
      // compiler does not refine `Type`
      const target = _target as FriendlyId;

      const result = modifyAndGet(state,
        x => x.frUnits, x => {
          const result = removeUnit(x, target);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "enemy": {
      // compiler does not refine `Type`
      const target = _target as EnemyId;

      const result = modifyAndGet(state,
        x => x.enUnits, x => {
          const result = removeUnit(x, target);
          return { a: result.row, b: result.entity };
        }
      );

      return { state: result.s, entity: result.b };
    }
    case "status": {
      // compiler does not refine `Type`
      const target = _target as StatusId;

      for (const group of groupOrder) {
        const result = modifyAndGet(state,
          x => x.statusRows[group], x => {
            const result = removeStatus(x, target);
            return { a: result.row, b: result.entity };
          }
        );
        if (result.b !== undefined) {
          return { state: result.s, entity: result.b };
        }
      }

      return { state };
    }
    default: {
      throw "GameState.overTarget: impossible case";
    }
  }
}

export function possibleActions(
  state: GameState,
): SolutionData[] {
  const actions: SolutionData[] = [];
  state.frUnits.units.forEach(frUnit => {
    if (frUnit !== undefined) {
      frUnit.abilities.forEach(ability => {
        if (ability.inputs.length === 0) {
          actions.push({
            ability: ability.ability,
            origin: frUnit.id,
            inputs: [],
          });
        } else {
          const inputPossibilities = ability.inputs.map(input => {
            return possibleTargets(state, input);
          });
          const inputCartesian = cartesian(inputPossibilities);
          inputCartesian.forEach(inputPossibility => {
            actions.push({
              ability: ability.ability,
              origin: frUnit.id,
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
    case "TargetInput": {
      return possibleTargets(state, new UnitInput)
        .concat(possibleTargets(state, new StatusInput));
    }
    case "UnitInput": {
      return possibleTargets(state, new FriendlyInput)
        .concat(possibleTargets(state, new EnemyInput));
    }
    case "StatusInput": {
      const statusIds: TargetId[] = [];
      groupOrder.forEach(tag => {
        state.statusRows[tag].statuses.forEach(status => {
          statusIds.push(status.id);
        });
      });
      return statusIds;
    }
    case "FriendlyInput": {
      const frIds: TargetId[] = frFiltered(state).map(x => x.e.id);
      return frIds;
    }
    case "EnemyInput": {
      const enIds: TargetId[] = enFiltered(state).map(x => x.e.id);
      return enIds;
    }
  }
}

export function showGamestate(
  state: GameState,
): string {
  const fr = frFiltered(state).map(x => showUnit(x.e, x.i)).join(" | ");
  const en = enFiltered(state).map(x => showUnit(x.e, x.i)).join(" | ");
  const tr = statusById(state);
  const frTr = tr.fr.map((l, i) => {
    return `${i}: ${l.map(x => showStatus(x)).join(" | ")}`;
  }).join("\n");
  const enTr = tr.en.map((l, i) => {
    return `${i}: ${l.map(x => showStatus(x)).join(" | ")}`;
  }).join("\n");

  return `state:${state.type}\n${fr}\n${en}\n${frTr}\n${enTr}`;
}

export function statusById(
  state: GameState,
): {
  fr: StStatus[][],
  en: StStatus[][],
} {
  const fr: StStatus[][] = [];
  const en: StStatus[][] = [];
  groupOrder.forEach(tag => {
    state.statusRows[tag].statuses.forEach(status => {
      try {
        const position = statusPosition(state.statusRows[tag], status.id);
        if (position === undefined) {
          throw `triggerById: should not happen, id ${JSON.stringify(status.id)} does not exist`;
        }
        const ownerType = status.owner.type;
        switch (ownerType) {
          case "friendly": {
            if (fr[status.owner.id] === undefined) {
              fr[status.owner.id] = [status];
            } else {
              fr[status.owner.id].push(status);
            }
            break;
          }
          case "enemy": {
            if (en[status.owner.id] === undefined) {
              en[status.owner.id] = [status];
            } else {
              en[status.owner.id].push(status);
            }
            break;
          }
        }
      } catch (e) {
        
      }
    });
  });
  return { fr, en };
}