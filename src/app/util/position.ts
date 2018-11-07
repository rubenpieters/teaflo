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

export function relativeIn(
  parentWidth: number,
  parentHeight: number,
  xPct: number,
  width: number,
  yPct: number,
  height: number,
): Position {
  return {
    xMin: (xPct * parentWidth / 100) - (width / 2),
    xMax: (xPct * parentWidth / 100) + (width / 2),
    yMin: (yPct * parentHeight / 100) - (height / 2),
    yMax: (yPct * parentHeight / 100) + (height / 2),
  }
}

export function absoluteIn(
  pos: Position,
  parentWidth: number,
  parentHeight: number,
  xPct: number,
  width: number,
  yPct: number,
  height: number,
): Position {
  return {
    xMin: pos.xMin + (xPct * parentWidth / 100) - (width / 2),
    xMax: pos.xMin + (xPct * parentWidth / 100) + (width / 2),
    yMin: pos.yMin + (yPct * parentHeight / 100) - (height / 2),
    yMax: pos.yMin + (yPct * parentHeight / 100) + (height / 2),
  }
}

export function inPosition(
  pos: Position,
  x: number,
  y: number,
) {
  return x >= pos.xMin && x <= pos.xMax && y >= pos.yMin && y <= pos.yMax;
}