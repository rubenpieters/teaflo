import { config } from "src/app/config";

export type Position = {
  xMin: number,
  xMax: number,
  yMin: number,
  yMax: number,
}

export function createPosition(
  fromX: "left" | "right",
  x: number,
  width: number,
  fromY: "bot" | "top",
  y: number,
  height: number,
): Position {
  return {
    xMin: fromX === "left" ? x : config.gameWidth - x - width,
    xMax: fromX === "left" ? x + width : config.gameWidth - x,
    yMin: fromY === "top" ? y : config.gameHeight - y - height,
    yMax: fromY === "top" ? y + height : config.gameHeight - y,
  }
}