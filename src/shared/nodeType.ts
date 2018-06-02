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
      gain: { color: "Basic", type: "Temp", amount: 1 },
    },
    {
      tag: "GainEffect",
      gain: { color: "Stack", type: "Temp", amount: 2 },
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
      gain: { color: "Stack", type: "Temp", amount: 3 },
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
        maxCharges: 4,
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
      gain: { color: "Basic", type: "Temp", amount: 3 },
    }
    ],
    meta: {
      id: 203,
      name: "Basic 3",
      color: 0xAAAAAA,
    }
  },
  resource_t2_4: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "ConvertEffect",
      converts:
        {
          tag: "ConvertUnit",
          from: {
            color: "Stack",
            type: "Temp",
          },
          to: {
            color: "Stack",
            type: "Total",
          },
          amount: 1,
        }
    }
    ],
    meta: {
      id: 204,
      name: "Persist 1 Stack",
      color: 0xAAAAAA,
    }
  },
  resource_t2_5: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "Persister",
          cap: 5,
        },
      }
    }
    ],
    meta: {
      id: 205,
      name: "Persister 5",
      color: 0xAAAAAA,
    }
  },
  resource_t2_6: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 4,
        chargePerUse: 1,
        maxCharges: 4,
        modifierEffect: {
          tag: "IncreaseLoss",
          value: 1,
        },
      }
    },
    {
      tag: "AddModifier",
      modifier: {
        charges: 4,
        chargePerUse: 1,
        maxCharges: 4,
        modifierEffect: {
          tag: "IncreaseGain",
          value: 1,
        },
      }
    },
    ],
    meta: {
      id: 206,
      name: "Inc Gain/Inc Loss Mod",
      color: 0xAAAAAA,
    }
  },
  resource_t2_7: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "GainChargeEffect",
      value: 1,
    },
    ],
    meta: {
      id: 207,
      name: "Gain Charge 1",
      color: 0xAAAAAA,
    }
  },
  resource_t2_8: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        maxCharges: 1,
        modifierEffect: {
          tag: "DuplicateAddMod",
        },
      }
    },
    ],
    meta: {
      id: 208,
      name: "DuplicateAddMod",
      color: 0xAAAAAA,
    }
  },
  resource_t3_1: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "ConvertEffect",
      converts:
        {
          tag: "ConvertUnit",
          from: {
            color: "Stack",
            type: "Temp",
          },
          to: {
            color: "Stack",
            type: "Total",
          },
          amount: 3,
        }
    }
    ],
    meta: {
      id: 301,
      name: "Persist 3 Stack",
      color: 0xAAAAAA,
    }
  },
  resource_t3_2: {
    tag: "ResourceNode",
    effects: [
    {
      tag: "GainEffect",
      gain: { color: "Basic", type: "Temp", amount: 5 },
    }
    ],
    meta: {
      id: 302,
      name: "Basic 5",
      color: 0xAAAAAA,
    }
  },
};