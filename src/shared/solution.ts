import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { allNodes } from "src/shared/nodeType";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import { NodeEffect } from "src/shared/nodeEffect";
import { ResourceType, ResourceColor } from "src/shared/resourceType";
import * as Phaser from "phaser-ce";

export function verifyAndAddConnection(from: Node, to: Node, connectionId: number, validFromNodes: number[], solution: Solution): ConnectResult {
  // TODO: use Array.contains ?
  if (validFromNodes.filter(x => x === from.id).length < 1) {
    return { tag: "InvalidFromNode" };
  }

  if (from.nodeType.tag !== "StartNode") {
    const angleCenter: number = Phaser.Math.radToDeg(Phaser.Math.angleBetween(0, 0, from.x, from.y));
    const angleNewLine: number = Phaser.Math.radToDeg(Phaser.Math.angleBetween(from.x, from.y, to.x, to.y));
    const verifyAngle = Phaser.Math.wrapAngle(angleCenter - angleNewLine);
    if (verifyAngle > 90 || verifyAngle < -90) {
      return { tag: "InvalidAngle" };
    }
  }

  validFromNodes.push(to.id);
  const currentConnections: Connection[] | undefined = solution[from.id];
  if (currentConnections !== undefined) {
    currentConnections.push({ to: to, distance: 0.0, connectionId: connectionId });
  } else {
    solution[from.id] = [{ to: to, distance: 0.0, connectionId: connectionId }];
  }

  return { tag: "ValidConnection", newValidFromNodes: validFromNodes, newSolution: solution };
}

type SuccessRunResult = {
  tag: "SuccessRunResult",
  result: StepResult,
};

type FailRunResult = {
  tag: "FailRunResult",
};

type RunResult = SuccessRunResult | FailRunResult;

const emptyResource: () => Resource = function() {
  return {
    "Temp": 0,
    "Total": 0,
  };
};

const startResources: () => RunResources = function() {
  return {
    "Basic": emptyResource(),
    "Red": emptyResource(),
    "Green": emptyResource(),
    "Blue": emptyResource(),
    "Yellow": emptyResource(),
    "Victory": emptyResource(),
  };
};

type Resource = {
  "Temp": number,
  "Total": number,
};

type RunResources = {
  "Basic": Resource,
  "Red": Resource,
  "Green": Resource,
  "Blue": Resource,
  "Yellow": Resource,
  "Victory": Resource,
};

type EffectFunction = (resources: RunResources) => RunResources;

type StepResult = {
  resources: RunResources,
  nodeId: number,
  count: number,
};

type StepResultX = {
  resources: RunResources,
  nodeId: number,
};

export function initVisit(solution: Solution, limit: number): StepResultX {
  const startNode = { id: 0, x: 0, y: 0, nodeType: allNodes.startNode };
  return visitStep(solution, startNode, startResources(), [], limit, 0);
}

function visitStep(solution: Solution, node: Node, resources: RunResources, nextNodes: Node[], limit: number, count: number): StepResultX {
  const visitResult = visitNode(solution, node);

  const newNextNodes = visitResult.next.map(conn => conn.to).concat(nextNodes);
  let newResources = resources;

  for (const effect of visitResult.effects) {
    newResources = effectFunction(effect)(newResources);
  }

  if (count + 1 >= limit) {
    return { resources: newResources, nodeId: node.id };
  } else if (newNextNodes.length === 0) {
    return { resources: newResources, nodeId: node.id };
  } else {
    const [nextNode] = newNextNodes.splice(0, 1);
    return visitStep(solution, nextNode, newResources, newNextNodes, limit, count + 1);
  }
}

function visitNode(solution: Solution, node: Node) {
  const connections: Connection[] | undefined = solution[node.id];

  if (connections === undefined) {
    // use final effect
    const effect = node.nodeType.finalEffect;

    // clear temporary resources

    return { next: [], effects: effect };
  } else {
    if (connections.length > 0) {
      // use link effect
      const effect = node.nodeType.linkEffect;

      return { next: connections, effects: effect };
    } else {
      throw "Should not happen: " + node.id + " has empty connections";
    }
  }

}

export function effectFunction(effect: NodeEffect): EffectFunction {
  switch (effect.tag) {
    case "NilEffect": {
      return resources => { return resources; };
    }
    case "GainEffect": {
      return resources => {
        const newResources: RunResources = Object.assign({}, resources);
        for (const gain of effect.gains) {
          newResources[gain.color][gain.type] = newResources[gain.color][gain.type] + gain.amount;
        }
        return newResources;
      };
    }
    case "ClearTemp": {
      return resources => {
        const newResources: RunResources = Object.assign({}, resources);
        for (const resourceColor of allColors) {
          newResources[resourceColor]["Temp"] = 0;
        }
        return newResources;
      }
    }
    case "ConsumeEffect": {
      return resources => {
        // TODO: check and consume should fetch from temp/total pool together
        if (checkResources(resources, effect.consume)) {
          let newResources: RunResources = Object.assign({}, resources);
          for (const consumeEff of effect.afterConsume) {
            newResources = effectFunction(consumeEff)(newResources)
          }
          return newResources;
        } else {
          return resources;
        }
      }
    }
  }
}

function checkResources(resources: RunResources, toCheck: {
  color: ResourceColor,
  type: ResourceType,
  amount: number,
}[]): boolean {
  for (const res of toCheck) {
    if (resources[res.color][res.type] < res.amount) {
      return false;
    }
  }
  return true;
}

const allColors: ResourceColor[] = ["Basic", "Red", "Green", "Blue", "Yellow", "Victory"];

function clearResourceTypes(types: ResourceType[], resources: RunResources): void {
  for (const resourceType of types) {
    for (const resourceColor of allColors) {
      resources[resourceColor][resourceType] = 0;
    }
  }
}