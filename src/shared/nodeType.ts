import { Resource } from "src/shared/resource"

type StartNode = {
  tag: "StartNode",
};

type ResourceNode = {
  tag: "ResourceNode",
  cost: Resource,
  gain: Resource,
};

type VictoryNode = {
  tag: "VictoryNode",
  cost: Resource,
};

export type NodeType = StartNode | ResourceNode | VictoryNode;
