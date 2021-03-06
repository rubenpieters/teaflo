import { stStatusPosition, position } from "../../../shared/game/state";
import { FriendlyId, EnemyId, StatusId } from "../../../shared/definitions/entityId";
import { createPosition, relativeTo, Position } from "../../../app/util/position";
import { groupOrder } from "../../../shared/game/status";
import { GameState } from "../../../shared/definitions/state";
import { StStatus } from "src/shared/definitions/statusRow";
import deepEqual = require("deep-equal");
import { Settings } from "src/app/data/settings";

/*

1: log - (40x40 icons)
2: aether - (40x40 icons)
3: units
4: tree
5: menu

+-+ +------+
|1| |222222|
|1| +------+
|1| +------+
|1| |333333|
+-+ +------+
+----+ +---+
|4444| |555|
+----+ +---+
*/

// 3 - units

const unitSizeX = 150;
const unitSizeY = 150;
const unitSpacing = 30;
const unitSpaceNeeded = unitSizeX * 4 + unitSpacing * 3;

export const unitFrMinY = 300;
const unitFrMinX = 260;
const unitFrMaxX = unitFrMinX + unitSpaceNeeded;

const unitEnMinY = 45;
export const unitEnMinX = unitFrMaxX + 130;
const unitEnMaxX = unitEnMinX + unitSpaceNeeded;

export const explX = unitEnMinX;
export const explY = unitEnMinY + 470;
export const explArrowEnd = { x: explX - 30, y: explY + 40 };

export function friendlyUnitPos(
  settings: Settings,
  state: GameState,
  unitId: FriendlyId | number,
) {
  let frPosition: number;
  if (typeof unitId !== "number") {
    const result = position(state, unitId);
    if (result === undefined) {
      throw `friendlyUnitPos: unexpected nonexisting unit ${JSON.stringify(unitId)}`;
    }
    frPosition = result;
  } else {
    frPosition = unitId;
  }
  return createPosition(settings,
    "left", unitFrMinX + 205 * frPosition, unitSizeX,
    "top", unitFrMinY, unitSizeY,
  );
}

export function enemyUnitPos(
  settings: Settings,
  state: GameState,
  unitId: EnemyId | number,
) {
  let enPosition: number;
  if (typeof unitId !== "number") {
    const result = position(state, unitId);
    if (result === undefined) {
      throw `enemyUnitPos: unexpected nonexisting unit ${JSON.stringify(unitId)}`;
    }
    enPosition = result;
  } else {
    enPosition = unitId;
  }
  return createPosition(settings,
    "left", unitEnMinX + 205 * enPosition, unitSizeX,
    "top", unitEnMinY, unitSizeY,
  );
}

export function unitUtilityPositions(
  unitPos: Position,
): {
  hpIconPos: Position,
  hpTextPos: Position,
  unitHpPos: Position,
  chIconPos: Position,
  chTextPos: Position,
  unitChPos: Position,
} {
  const hpIconPos = relativeTo(unitPos,
    [{ type: "below", amt: 210 }],
    40, 40,
  );
  const hpTextPos = relativeTo(hpIconPos,
    [{ type: "right", amt: 10 }, { type: "above", amt: -30 }],
    50, 20,
  );
  const chIconPos = relativeTo(unitPos,
    [{ type: "below", amt: 250 }],
    40, 40,
  );
  const chTextPos = relativeTo(chIconPos,
    [{ type: "right", amt: 10 }, { type: "above", amt: -30 }],
    50, 20,
  );
  const unitHpPos = relativeTo(unitPos,
    [{ type: "left", amt: -5 }, { type: "below", amt: 25 }],
    10, 150,
  );
  const unitChPos = relativeTo(unitPos,
    [{ type: "right", amt: 2 }, { type: "below", amt: 25 }],
    10, 150,
  );
  return { hpIconPos, hpTextPos, unitHpPos, chIconPos, chTextPos, unitChPos, };
}

export function statusPos(
  settings: Settings,
  state: GameState,
  statusesById: { fr: StStatus[][], en: StStatus[][], },
  statusOrder: "byOrder" | "byId",
  statusId: StatusId,
  // x index, the index within its status row
  columnPosition?: number,
  // y index, to which status row it belongs
  rowPosition?: number,
): Position {
  if (statusOrder === "byOrder") {
    if (columnPosition === undefined || rowPosition === undefined) {
      const result = stStatusPosition(state, statusId);
      if (result === undefined) {
        throw `enemyUnitPos: unexpected nonexisting unit ${JSON.stringify(statusId)}`;
      }
      columnPosition = result.columnPosition;
      rowPosition = result.rowPosition;
    }
    return createPosition(settings,
      "left", 240 + 50 * columnPosition, 40,
      "top", 50 + 50 * rowPosition, 40,
    );
  } else if (statusOrder === "byId") {
    let statusInfo = findStatus(statusesById.fr, statusId);
    let basePos: Position = undefined as any;
    if (statusInfo === undefined) {
      statusInfo = findStatus(statusesById.en, statusId);
    } else {
      basePos = friendlyUnitPos(settings, state, statusInfo.unitId);
      return relativeTo(basePos,
        [{ type: "above", amt: 100 }, { type: "right", amt: 40 * statusInfo.stId }],
        40, 40,
      );
    }
    if (statusInfo === undefined) {
      throw `can not find ${JSON.stringify(statusId)}`;
    } else {
      basePos = enemyUnitPos(settings, state, statusInfo.unitId);
    }

    return relativeTo(basePos,
      [{ type: "above", amt: 100 }, { type: "right", amt: 40 * statusInfo.stId }],
      40, 40,
    );
  } else {
    throw "statusPos: should not happen";
  }
}

function findStatus(
  rows: StStatus[][],
  statusId: StatusId,
) {
  let unitId = 0;
  for (const row of rows) {
    let stId = 0;
    for (const st of row) {
      if (deepEqual(statusId, st.id)) {
        return { unitId, stId };
      }
      stId += 1;
    }
    unitId += 1;
  }
  return undefined;
}

export function logPosition(
  settings: Settings,
  entryIndex: number,
  typeIndex: number,
) {
  return createPosition(settings,
    "left", 22 + 39 * entryIndex, 39,
    "top", 39 + 39 * typeIndex, 39,
  );
}

export function sourceUnitPos(
): Position {
  return { xMin: 650, yMin: 600, xMax: 800, yMax: 750 };
}

export function targetUnitPos(
): Position {
  return { xMin: 850, yMin: 600, xMax: 1000, yMax: 750 };
}