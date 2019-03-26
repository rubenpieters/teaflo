import { GameState, findStatus } from "../../../shared/game/state";
import { EntityId, toPositionId, GlobalId } from "../../../shared/game/entityId";
import { createPosition, relativeTo, Position } from "../../../app/util/position";
import { triggerOrder } from "../../../shared/game/trigger";

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

const unitMinY = 450;

const unitFrMinX = 250;
const unitFrMaxX = unitFrMinX + unitSpaceNeeded;

const unitEnMinX = unitFrMaxX + 100;
const unitEnMaxX = unitEnMinX + unitSpaceNeeded;

export function friendlyUnitPos(
  state: GameState,
  unitId: EntityId<"friendly">,
) {
  const positionId = toPositionId(state, unitId);
  return createPosition(
    "left", unitFrMinX + 170 * positionId.id, unitSizeX,
    "top", unitMinY, unitSizeY,
  );
}

export function enemyUnitPos(
  state: GameState,
  unitId: EntityId<"enemy">,
) {
  const positionId = toPositionId(state, unitId);
  return createPosition(
    "left", unitEnMinX + 170 * positionId.id, unitSizeX,
    "top", unitMinY, unitSizeY,
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
    [{ type: "below", amt: 10 }],
    40, 40,
  );
  const hpTextPos = relativeTo(hpIconPos,
    [{ type: "right", amt: 10 }, { type: "above", amt: -30 }],
    50, 20,
  );
  const chIconPos = relativeTo(unitPos,
    [{ type: "below", amt: 50 }],
    40, 40,
  );
  const chTextPos = relativeTo(chIconPos,
    [{ type: "right", amt: 10 }, { type: "above", amt: -30 }],
    50, 20,
  );
  const unitHpPos = relativeTo(unitPos,
    [{ type: "left", amt: -5 }],
    10, 150,
  );
  const unitChPos = relativeTo(unitPos,
    [{ type: "right", amt: -5 }],
    10, 150,
  );
  return { hpIconPos, hpTextPos, unitHpPos, chIconPos, chTextPos, unitChPos, };
}

export function statusPos(
  state: GameState,
  statusId: GlobalId<"status">,
  // x index, the index within its status group
  triggerIndex?: number,
  // y index, to which status group it belongs
  tagIndex?: number,
) {
  if (triggerIndex === undefined || tagIndex === undefined) {
    const index = findStatus(state, statusId);
    triggerIndex = index!.index;
    tagIndex = triggerOrder.findIndex(x => x === index!.group);
  }
  return createPosition(
    "left", 240 + 50 * triggerIndex, 40,
    "top", 50 + 50 * tagIndex, 40,
  );
}