import { Resource } from "src/shared/resource";
import { NodeEffect, NilEffect } from "src/shared/nodeEffect";

type NodeTypeMeta = {
  id: number,
  name: string,
  color: number,
};

type StartNode = {
  tag: "StartNode",
  linkEffect: NodeEffect[],
  finalEffect: NodeEffect[],
  meta: NodeTypeMeta,
};

type ResourceNode = {
  tag: "ResourceNode",
  linkEffect: NodeEffect[],
  finalEffect: NodeEffect[],
  meta: NodeTypeMeta,
};

export type NodeType = StartNode | ResourceNode;

// declaration of all node types

export const allNodes: { [key: string]: NodeType } = {
  startNode: {
    tag: "StartNode",
    linkEffect: [],
    finalEffect: [],
    meta: {
      id: 0,
      name: "Start Node",
      color: 0xFFFFFF,
    }
  },
  twoBasicBranch: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 2 }],
    }],
    finalEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Victory", type: "Total", amount: 1 }],
    },
    { tag: "ClearTemp" }
    ],
    meta: {
      id: 1,
      name: "2 x (temp)",
      color: 0xAAAAAA,
    }
  },
  oneBasicFork: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Victory", type: "Total", amount: 1 }],
    },
    { tag: "ClearTemp" }
    ],
    meta: {
      id: 2,
      name: "1 x (temp)",
      color: 0xAAAAAA,
    }
  },
  twoRedTemp: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Red", type: "Temp", amount: 2 }],
    }],
    finalEffect: [{
      tag: "ClearTemp",
    }],
    meta: {
      id: 3,
      name: "1 R (temp)",
      color: 0xAAAAAA,
    }
  },
  consume1RTo3R: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Red", type: "Temp", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Red", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [{
      tag: "ClearTemp",
    }],
    meta: {
      id: 3,
      name: "consume 1 R to 3R",
      color: 0xAAAAAA,
    }
  }
};
