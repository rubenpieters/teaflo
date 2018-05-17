import { Resource } from "src/shared/resource";
import { NodeEffect } from "src/shared/rules/effect";

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
  resource1: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Total", amount: 1 }],
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 7,
      name: "1 Basic",
      color: 0xAAAAAA,
    }
  },
};

/*
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
      color: 0xFF0000,
    }
  },
  consume1RTo3R: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Red", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Red", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [{
      tag: "ClearTemp",
    }],
    meta: {
      id: 4,
      name: "consume 1 R to 3R",
      color: 0xAAAAAA,
    }
  },
  persistAll: {
    tag: "ResourceNode",
    linkEffect: [
    ],
    finalEffect: [{
      tag: "PersistEffect",
    }],
    meta: {
      id: 5,
      name: "persist",
      color: 0xAAFFFF,
    }
  },
  ignoreNextConsume: {
    tag: "ResourceNode",
    linkEffect: [
    ],
    finalEffect: [{
      tag: "AddModifier",
      modifierType: {
        tag: "OneTimeModifier",
        effect: {
          tag: "IgnoreNextConsume",
        }
      }
    }],
    meta: {
      id: 6,
      name: "ignore next consume",
      color: 0xAAFFAA,
    }
  },
  resource1: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Total", amount: 1 }],
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 7,
      name: "1 Basic",
      color: 0xAAAAAA,
    }
  },
  resource2_1: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [
    {
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Total", amount: 3 }],
      }]
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 8,
      name: "Consume 1: Gain 3",
      color: 0xAAAAAA,
    }
  },
  resource3_1: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 3 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 5 }],
      }]
    }],
    finalEffect: [
    {
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 3 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Total", amount: 5 }],
      }]
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 8,
      name: "Consume 3: Gain 5",
      color: 0xAAAAAA,
    }
  },
};
*/