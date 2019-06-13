import { EnemyId } from "../definitions/entityId";
import { over, focus } from "../iassign-util";
import { HasThreatMap } from "../definitions/threat";

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

export function removeThreat<U extends HasThreatMap>(
  u: U,
  atEnemy: EnemyId,
  value: number,
): U {
  return focus(u,
    over(x => x.threatMap[atEnemy.id],
      x => x === undefined ? value : Math.max(0, x - value)
    ),
  );
}