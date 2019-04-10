import { EnemyId } from "./entityId";
import { over, focus } from "../iassign-util";

export type HasThreatMap = {
  threatMap: ThreatMap,
}

export type ThreatMap = { [globalId: number]: number };

export function addThreat<U extends HasThreatMap>(
  u: U,
  atEnemy: EnemyId,
  value: number,
): U {
  return focus(u,
    over(x => x.threatMap[atEnemy.id],
      x => x === undefined ? value : x + value
    ),
  );
}