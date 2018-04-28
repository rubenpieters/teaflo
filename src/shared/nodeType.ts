import { Resource } from "src/shared/resource";
import { NodeEffect } from "src/shared/nodeEffect";

type NodeTypeMeta = {
  id: number,
  name: string,
  color: number,
};

type StartNode = {
  tag: "StartNode",
  meta: NodeTypeMeta,
};

type ResourceNode = {
  tag: "ResourceNode",
  linkEffect: NodeEffect,
  finalEffect: NodeEffect,
  meta: NodeTypeMeta,
};

export type NodeType = StartNode | ResourceNode;

// declaration of all node types

export const allNodes: { [key: string]: NodeType } = {
  startNode: {
    tag: "StartNode",
    meta: {
      id: 0,
      name: "Start Node",
      color: 0xFFFFFF,
    }
  },
  twoBasicBranch: {
    tag: "ResourceNode",
    linkEffect: {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Branch", amount: 2 }],
    },
    finalEffect: {
      tag: "NilEffect",
    },
    meta: {
      id: 1,
      name: "2 I (basic)",
      color: 0xAAAAAA,
    }
  },
  oneBasicFork: {
    tag: "ResourceNode",
    linkEffect: {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Fork", amount: 1 }],
    },
    finalEffect: {
      tag: "NilEffect",
    },
    meta: {
      id: 2,
      name: "1 Y (basic)",
      color: 0xAAAAAA,
    }
  }
};
