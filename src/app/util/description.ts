import { DescToken } from "src/shared/definitions/description";
import { Pool } from "../phaser/pool";

export function groupFromDesc<Data, FrameType>(
  tokens: DescToken[],
  size: number,
  parent: { x: number, y: number },
  mkFrameType: () => FrameType,
  mkData: (sprite: string) => Data,
  pool: Pool<Data, FrameType>,
) {
  let y = 0;
  let xOffset = 0;
  const dataList: {
    x: number,
    y: number,
    frameType: FrameType,
    data: Data,
  }[] = [];
  tokens.forEach((descSym, descIndex) => {
    switch (descSym.tag) {
      case "DescSeparator": {
        y += 1;
        xOffset = descIndex + 1;
        break;
      }
      case "DescSymbol": {
        const spriteName = `${descSym.symbol}.png`;
        const xPos = 80 * (descIndex - xOffset);
        const yPos = - y * 80;
        dataList.push({ x: xPos, y: yPos, frameType: mkFrameType(), data: mkData(spriteName) });
        break;
      }
    }
  });
  return pool.newGroup(parent.x, parent.y, dataList);
}