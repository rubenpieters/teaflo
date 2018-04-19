type StartNode = {
  tag: "StartNode"
};

type ResourceNode = {
  tag: "ResourceNode"
};

type VictoryNode = {
  tag: "VictoryNode"
};

export type NodeType = StartNode | ResourceNode | VictoryNode;
