import { EntityId, TargetType } from "../definitions/entityId";

export function showEntityId(
  id: EntityId<TargetType>
) {
  switch (id.type) {
    case "friendly": return `FID ${id.id}`;
    case "enemy": return `EID ${id.id}`;
    case "status": return `SID ${id.id}`;
  }
}