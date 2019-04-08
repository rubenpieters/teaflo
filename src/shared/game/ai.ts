export type AIPosition = {
  x: number,
  y: number,
}

export type AIDirection
  = "up"
  | "down"
  | "left"
  | "right"
  ;

export function moveAI(
  aiPos: AIPosition,
  dir: AIDirection,
): AIPosition {
  switch (dir) {
    case "up": return { x: aiPos.x, y: aiPos.y - 1 };
    case "down": return { x: aiPos.x, y: aiPos.y + 1 };
    case "left": return { x: aiPos.x - 1, y: aiPos.y };
    case "right": return { x: aiPos.x + 1, y: aiPos.y };
  }
}

/**
 * positions:
 * 0 - 1 - 2
 * 3 - 4 - 5
 * 6 - 7 - 8
 */

export function aiPosToIndex(
  aiPos: AIPosition
): number {
  return aiPos.x + aiPos.y * 3;
}

export function indexToAiPos(
  index: number,
): AIPosition {
  const x = index % 3;
  const y =  Math.round((index / 3) - 0.5);
  return { x, y };
}

export const aiIndices = [0,1,2,3,4,5,6,7,8];

export function aiPositions(
): AIPosition[] {
  return aiIndices.map(indexToAiPos);
}