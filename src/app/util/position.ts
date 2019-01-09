import { settings } from "../data/settings";

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
    xMin: fromX === "left" ? x : settings.gameWidth - x - width,
    xMax: fromX === "left" ? x + width : settings.gameWidth - x,
    yMin: fromY === "top" ? y : settings.gameHeight - y - height,
    yMax: fromY === "top" ? y + height : settings.gameHeight - y,
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

export function relativeTo(
  parentPos: Position,
  relativeType: "below" | "above" | "right" | "left",
  relativeAmt: number,
  width: number,
  height: number,
): Position {
  switch (relativeType) {
    case "below": {
      return {
        xMin: parentPos.xMin,
        xMax: parentPos.xMin + width,
        yMin: parentPos.yMax + relativeAmt,
        yMax: parentPos.yMax + relativeAmt + height,
      }
    }
    case "above": {
      return {
        xMin: parentPos.xMin,
        xMax: parentPos.xMin + width,
        yMin: parentPos.yMin - relativeAmt - height,
        yMax: parentPos.yMin - relativeAmt,
      }
    }
    case "right": {
      return {
        xMin: parentPos.xMax + relativeAmt,
        xMax: parentPos.xMax + relativeAmt + width,
        yMin: parentPos.yMin,
        yMax: parentPos.yMin + height,
      }
    }
    case "left": {
      return {
        xMin: parentPos.xMin - relativeAmt - width,
        xMax: parentPos.xMax - relativeAmt,
        yMin: parentPos.yMin,
        yMax: parentPos.yMin + height,
      }
    }
  }
}