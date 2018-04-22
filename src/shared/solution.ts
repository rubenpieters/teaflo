import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";

export function verifyAndAddConnection(from: Node, to: Node, validFromNodes: number[], solution: Solution): ConnectResult {
  // TODO: use Array.contains ?
  if (validFromNodes.filter(x => x === from.id).length < 1) {
    return { tag: "InvalidFromNode" };
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