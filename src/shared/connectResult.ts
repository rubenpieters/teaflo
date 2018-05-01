import { Node } from "src/shared/node";

export type Connection = {
  to: Node,
  distance: number,
  connectionId: number,
};

export type Solution = {
  [id: number]: Connection[],
};

type InvalidFromNode = {
  tag: "InvalidFromNode",
}

type InvalidAngle = {
  tag: "InvalidAngle",
}

type ValidConnection = {
  tag: "ValidConnection",
  newValidFromNodes: number[],
  newSolution: Solution;
}

export type ConnectResult = InvalidFromNode | InvalidAngle | ValidConnection;