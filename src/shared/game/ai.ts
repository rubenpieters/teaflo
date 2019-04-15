import { AIPosition, AIDirection } from "../definitions/ai";

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

export function routeDirectionDescription(
  aiDirection: AIDirection,
) {
  switch (aiDirection) {
    case "down": return "icon_ai_down";
    case "left": return "icon_ai_left";
    case "right": return "icon_ai_right";
    case "up": return "icon_ai_up";
  }
}