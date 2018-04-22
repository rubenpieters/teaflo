import { Node } from "src/shared/node";

export type Connection = {
  to: Node,
  distance: number,
};

export type Solution = {
  [id: string]: Connection[],
};

type InvalidFromNode = {
  tag: "InvalidFromNode",
}

type ValidConnection = {
  tag: "ValidConnection",
  newValidFromNodes: number[],
  newSolution: Solution;
}

export type ConnectResult = InvalidFromNode | ValidConnection;