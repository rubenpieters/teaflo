import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import * as Phaser from "phaser-ce"

export function verifyAndAddConnection(from: Node, to: Node, validFromNodes: number[], solution: Solution): ConnectResult {
  // TODO: use Array.contains ?
  if (validFromNodes.filter(x => x === from.id).length < 1) {
    return { tag: "InvalidFromNode" };
  }

  if (from.nodeType.tag !== "StartNode") {
    const angleCenter: number = Phaser.Math.radToDeg(Phaser.Math.angleBetween(0, 0, from.x, from.y));
    const angleNewLine: number = Phaser.Math.radToDeg(Phaser.Math.angleBetween(from.x, from.y, to.x, to.y));
    const verifyAngle = Phaser.Math.wrapAngle(angleCenter - angleNewLine);
    if (verifyAngle > 90 || verifyAngle < -90) {
      return { tag: "InvalidAngle" };
    }
  }

  validFromNodes.push(to.id);
  const currentConnections: Connection[] | undefined = solution[from.id];
  if (currentConnections !== undefined) {
    currentConnections.push({ to: to, distance: 0.0 });
  } else {
    solution[from.id] = [{ to: to, distance: 0.0 }];
  }

  return { tag: "ValidConnection", newValidFromNodes: validFromNodes, newSolution: solution };
}