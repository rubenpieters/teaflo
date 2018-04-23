import { Resource } from "src/shared/resource"

type StartNode = {
  tag: "StartNode",
  color: number,
};

type ResourceNode = {
  tag: "ResourceNode",
  cost: Resource,
  gain: Resource,
  color: number,
};

type VictoryNode = {
  tag: "VictoryNode",
  cost: Resource,
  color: number,
};

export type NodeType = StartNode | ResourceNode | VictoryNode;
