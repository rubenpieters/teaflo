import { DescToken } from "../../shared/definitions/description";
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
        const spriteName = `${descSym.symbol}_${size}_${size}.png`;
        const xPos = size * (descIndex - xOffset);
        const yPos = y * size;
        dataList.push({ x: xPos, y: yPos, frameType: mkFrameType(), data: mkData(spriteName) });
        break;
      }
    }
  });
  return pool.newGroup(parent.x, parent.y, dataList);
}