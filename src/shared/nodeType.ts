import iassign from "immutable-assign";
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

function addClear(nodeType: ResourceNode): NodeType {
  const addedClear: NodeEffect[] = nodeType.linkEffect.concat([{ tag: "ClearTemp" }]);
  return iassign(nodeType,
    x => x.linkEffect,
    x => addedClear);
}

function addConsumeX(nodeType: ResourceNode, consumeAmt: number): NodeType {
  const addedConsumeLink: NodeEffect[] = [{
    tag: "ConsumeEffect",
    consume: [{ color: "Basic", type: "Both", amount: consumeAmt }],
    afterConsume: nodeType.linkEffect,
  }];
  const addedConsumeFinal: NodeEffect[] = [{
    tag: "ConsumeEffect",
    consume: [{ color: "Basic", type: "Both", amount: consumeAmt }],
    afterConsume: nodeType.finalEffect,
  }];
  return iassign(iassign(nodeType,
    x => x.linkEffect,
    x => addedConsumeLink),
    x => x.finalEffect,
    x => addedConsumeFinal);
}

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
  resource1_1: {
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
      id: 1,
      name: "1 Basic",
      color: 0xAAAAAA,
    }
  },
  resource1_2: {
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
      id: 2,
      name: "Consume 1: Gain 3",
      color: 0xAAAAAA,
    }
  },
  resource1_3: {
    tag: "ResourceNode",
    linkEffect: [{
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
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 2,
      name: "Consume 2: Gain 4",
      color: 0xAAAAAA,
    }
  },
  resource2_1: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        modifierEffect: {
          tag: "IgnoreNextConsume",
        }
      },
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 3,
      name: "Ignore Next Consume",
      color: 0xAAAAAA,
    }
  },
  resource2_2: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
    {
      tag: "AddModifier",
      modifier: {
        charges: 1,
        chargePerUse: 1,
        modifierEffect: {
          tag: "DoubleNextGain",
        }
      },
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 4,
      name: "Double Next Gain",
      color: 0xAAAAAA,
    }
  },
  resource_t3: {
    tag: "ResourceNode",
    linkEffect: [{
      tag: "GainEffect",
      gains: [{ color: "Basic", type: "Temp", amount: 1 }],
    }],
    finalEffect: [
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
    },
    {
      tag: "ClearTemp",
    }],
    meta: {
      id: 4,
      name: "Convert Basic to Victory",
      color: 0xAAAAAA,
    }
  },
};
