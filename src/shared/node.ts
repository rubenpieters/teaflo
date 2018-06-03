import { NodeType } from "src/shared/nodeType";

export type Node = {
  id: number,
  x: number,
  y: number,
  tier: number,
  nodeType: NodeType
};

export function satisfiesFilter(filter: string, node: Node): boolean {
  return node.nodeType.effects.some(e => e.tag === filter);
}