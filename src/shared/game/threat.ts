import { focus, over, set } from "../iassign-util";
import { UnitId, toGlobalId } from "./entityId";
import { GameState } from "./state";

export type HasThreatMap = {
  threatMap: ThreatMap,
}

export type ThreatMap = { [globalId: number]: number };

export function addThreat<U extends HasThreatMap>(
  u: U,
  state: GameState,
  atEnemy: UnitId,
  value: number,
): U {
  const globalId = toGlobalId(state, atEnemy);
  return focus(u, over(x => x.threatMap[globalId.id], x => x === undefined ? value : x + value));
}