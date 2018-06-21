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

export function addConnect(nodeType: NodeType): NodeType {
  const connectEffect: NodeEffect[] = [{ tag: "ConnectEffect" }];
  return iassign(nodeType, x => x.effects, x => connectEffect.concat(x));
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
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 3 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 99,
          chargePerUse: 0,
          maxCharges: 99,
          fragile: false,
          modifierEffect: {
            tag: "EffectOnConnect",
            effect: {
              tag: "GainEffect",
              gain: { color: "Stack", type: "Temp", amount: 2 },
            },
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 101,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t1_2: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 3 },
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
          fragile: false,
          modifierEffect: {
            tag: "DuplicateAddMod",
            value: 1,
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 102,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t1_5_1: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 1 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Stack", type: "Temp", amount: 1 },
      }
    ],
    meta: {
      id: 1501,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t2_1: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 3 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 5 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 3,
          chargePerUse: 1,
          maxCharges: 3,
          fragile: false,
          modifierEffect: {
            tag: "IncreaseGain",
            value: 2,
          },
        },
        amount: 1,
        location: "Back",
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
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 3 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 5 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 1,
          chargePerUse: 1,
          maxCharges: 1,
          fragile: false,
          modifierEffect: {
            tag: "DuplicateAddMod",
            value: 2,
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 202,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t2_3: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 3 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 5 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 10,
          chargePerUse: 1,
          maxCharges: 10,
          fragile: true,
          modifierEffect: {
            tag: "NoopMod",
          },
        },
        amount: 1,
        location: "Front",
      }
    ],
    meta: {
      id: 203,
      name: "",
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
        fragile: false,
        modifierEffect: {
          tag: "Persister",
          cap: 5,
        },
      },
      amount: 1,
      location: "Back",
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
        fragile: false,
        modifierEffect: {
          tag: "IncreaseLoss",
          value: 1,
        },
      },
      amount: 1,
      location: "Back",
    },
    {
      tag: "AddModifier",
      modifier: {
        charges: 4,
        chargePerUse: 1,
        maxCharges: 4,
        fragile: false,
        modifierEffect: {
          tag: "IncreaseGain",
          value: 1,
        },
      },
      amount: 1,
      location: "Back",
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
        fragile: false,
        modifierEffect: {
          tag: "DuplicateAddMod",
          value: 1,
        },
      },
      amount: 1,
      location: "Back",
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
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 5 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 7 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 3,
          chargePerUse: 1,
          maxCharges: 3,
          fragile: false,
          modifierEffect: {
            tag: "IncreaseGain",
            value: 3,
          },
        },
        amount: 1,
        location: "Back",
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
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 5 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 7 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 1,
          chargePerUse: 1,
          maxCharges: 1,
          fragile: false,
          modifierEffect: {
            tag: "DuplicateAddMod",
            value: 3,
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 302,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t3_3: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 5 },
      },
      {
        tag: "GainEffect",
        gain: { color: "Basic", type: "Temp", amount: 7 },
      },
      {
        tag: "DestroyModEffect",
        position: 0,
      },
      {
        tag: "GainChargeEffect",
        value: 5,
      }
    ],
    meta: {
      id: 302,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t4_1: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 7 },
      },
      {
        tag: "ConvertModsToVictory",
      }
    ],
    meta: {
      id: 401,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t4_2: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 7 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 1,
          chargePerUse: 1,
          maxCharges: 1,
          fragile: false,
          modifierEffect: {
            tag: "DuplicateAddMod",
            value: 4,
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 402,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t4_3: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 7 },
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 1,
          chargePerUse: 1,
          maxCharges: 1,
          fragile: false,
          modifierEffect: {
            tag: "GainXToVictory",
            minimum: 5,
            type: "Basic",
          },
        },
        amount: 1,
        location: "Back",
      }
    ],
    meta: {
      id: 403,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t4_4: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 7 },
      },
      {
        tag: "ConvertEffect",
        converts: {
          tag: "ConvertUnit",
          from: {
            color: "Stack",
            type: "Temp",
          },
          to: {
            color: "Stack",
            type: "Total",
          },
          amount: "All",
        }
      },
      {
        tag: "ConvertEffect",
        converts: {
          tag: "ConvertUnit",
          from: {
            color: "Basic",
            type: "Temp",
          },
          to: {
            color: "Basic",
            type: "Total",
          },
          amount: "All",
        }
      },
    ],
    meta: {
      id: 404,
      name: "",
      color: 0xAAAAAA,
    }
  },
  resource_t4_5: {
    tag: "ResourceNode",
    effects: [
      {
        tag: "LoseEffect",
        loss: { color: "Basic", type: "Both", amount: 7 },
      },
      {
        tag: "AllModsFragile",
      },
      {
        tag: "AddModifier",
        modifier: {
          charges: 99,
          chargePerUse: 0,
          maxCharges: 99,
          fragile: false,
          modifierEffect: {
            tag: "DuplicateAddMod",
            value: 1,
          },
        },
        amount: 1,
        location: "Back",
      },
    ],
    meta: {
      id: 405,
      name: "",
      color: 0xAAAAAA,
    }
  },
};