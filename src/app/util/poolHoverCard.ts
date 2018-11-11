import { Position } from "src/app/util/position";
import { config } from "../config";

export function createPoolHoverCard(
  pool: Phaser.Group,
  pos: Position,
  key: string,
): Phaser.Sprite {
  const hoverCard: Phaser.Sprite = pool.getFirstExists(false, true, pos.xMin, pos.yMin, key);

  hoverCard.width = config.hoverCardWidth;
  hoverCard.height = config.hoverCardHeight;

  return hoverCard;
}