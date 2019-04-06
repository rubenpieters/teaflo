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

export function aiPosToIndex(
  aiPos: AIPosition
): number {
  return aiPos.x + aiPos.y * 3;
}