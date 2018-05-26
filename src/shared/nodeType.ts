import iassign from "immutable-assign";
import { Resource } from "src/shared/resource";
import { NodeEffect, showEffect } from "src/shared/rules/effect";
import { Rng, chooseSet } from "src/shared/handler/rng/randomSeedRng";

type NodeTypeMeta = {
  id: number,
  name: string,
  color: number,
};

type StartNode = {
  tag: "StartNode",
  effects: NodeEffect[],
  meta: NodeTypeMeta,
};

type ResourceNode = {
  tag: "ResourceNode",
  effects: NodeEffect[],
  meta: NodeTypeMeta,
};

export type NodeType = StartNode | ResourceNode;

export function showNodeType(nodeType: NodeType): string {
  switch (nodeType.tag) {
    case "StartNode": {
      return "StartNode";
    }
    case "ResourceNode": {
      return "Resource: " + nodeType.effects.length + "\n" +
        nodeType.effects.map(showEffect).join("\n");
    }
  }
}

export function addNegative(nodeType: NodeType, negativeEffects: NodeEffect[], rng: Rng): NodeType {
  const randomEffect: NodeEffect = chooseSet(rng, negativeEffects);
  return iassign(nodeType, x => x.effects, x => [randomEffect].concat(x));
}

// declaration of all node types

export const allNodes: { [key: string]: NodeType } = {
  startNode: {
    tag: "StartNode",
    effects: [],
    meta: {
      id: 0,
      name: "Start Node",
      color: 0xFFFFFF,
    }
  },
  resource_t1_1: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "ClearTemp",
    },
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    },
    {
      tag: "GainEffect",
      gains: [{ color: "Stack", type: "Temp", amount: 2 }],
    }],
    meta: {
      id: 101,
      name: "1 Basic 1 Stack",
      color: 0xAAAAAA,
    }
  },
  resource_t1_2: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "ClearTemp",
    },
    {
      tag: "GainEffect",
      gains: [{ color: "Stack", type: "Temp", amount: 3 }],
    },
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "Buffer",
          value: 4,
        },
      }
    }],
    meta: {
      id: 102,
      name: "Buffer 1 - 4",
      color: 0xAAAAAA,
    }
  },
  resource_t2_1: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "Buffer",
          value: 8,
        },
      }
    }
    ],
    meta: {
      id: 201,
      name: "Buffer 1 - 8",
      color: 0xAAAAAA,
    }
  },
  resource_t2_2: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 4,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "Buffer",
          value: 2,
        },
      }
    }
    ],
    meta: {
      id: 202,
      name: "Buffer 4 - 2",
      color: 0xAAAAAA,
    }
  },
  resource_t2_3: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 3 }],
    }
    ],
    meta: {
      id: 203,
      name: "Basic 3",
      color: 0xAAAAAA,
    }
  },
  resource_t3_1: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }
    ],
    meta: {
      id: 301,
      name: "TODO",
      color: 0xAAAAAA,
    }
  },
};
/*
export const allNodes: { [key: string]: NodeType } = {
  startNode: {
    tag: "StartNode",
    effects: [],
    meta: {
      id: 0,
      name: "Start Node",
      color: 0xFFFFFF,
    }
  },
  resource_t1_1: {
    tag: "ResourceNode",
    effects: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
    {
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Total", amount: 1 }],
    }],
    meta: {
      id: 101,
      name: "1 Basic",
      color: 0xAAAAAA,
    }
  },
  resource_t2_1: {
    tag: "ResourceNode",
    effects: [{
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
    }],
    meta: {
      id: 201,
      name: "Consume 1: Gain 3",
      color: 0xAAAAAA,
    }
  },
  resource_t2_2: {
    tag: "ResourceNode",
    effects: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [
        {
          tag: "AddModifier",
          modifier: {
            charges: 1,
            chargePerUse: 1,
            modifierEffect: {
              tag: "IgnoreNextConsume",
            }
          },
        }]
    }],
    meta: {
      id: 202,
      name: "Ignore Next Consume",
      color: 0xAAAAAA,
    }
  },
  resource_t2_3: {
    tag: "ResourceNode",
    effects: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [
      {
        tag: "AddModifier",
        modifier: {
          charges: 1,
          chargePerUse: 1,
          modifierEffect: {
            tag: "DoubleNextGain",
          }
        },
      }]
    }],
    meta: {
      id: 203,
      name: "Double Next Gain",
      color: 0xAAAAAA,
    }
  },
  resource_t2_4: {
    tag: "ResourceNode",
    effects: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 3 }],
      }]
    }],
    finalEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 1 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 5 }],
      }]
    }],
    meta: {
      id: 204,
      name: "Consume 1: Gain 5",
      color: 0xAAAAAA,
    }
  },
  resource_t3_1: {
    tag: "ResourceNode",
    effects: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 2 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 4 }],
      }]
    }],
    finalEffect: [
    {
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 2 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Total", amount: 4 }],
      }]
    }],
    meta: {
      id: 301,
      name: "Consume 2: Gain 4",
      color: 0xAAAAAA,
    }
  },
  resource_t3_2: {
    tag: "ResourceNode",
    effects: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 2 }],
      afterConsume: [{
        tag: "GainEffect",
        gains: [{ color: "Basic", type: "Temp", amount: 4 }],
      }]
    }],
    finalEffect: [{
      tag: "ConsumeEffect",
      consume: [{ color: "Basic", type: "Both", amount: 2 }],
      afterConsume: [
        {
          tag: "ConvertEffect",
          converts: [{
            tag: "ConvertBothUnit",
            from: {
              color: "Basic",
            },
            to: {
              color: "Victory",
              type: "Total",
            },
            amount: "All",
          }]
        }]
    }],
    meta: {
      id: 302,
      name: "Convert Basic to Victory",
      color: 0xAAAAAA,
    }
  },
};
*/