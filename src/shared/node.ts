import { NodeType } from "src/shared/nodeType"

export type Node =
  { id: number,
    x: number,
    y: number,
    tier: number,
    nodeType: NodeType
  };
