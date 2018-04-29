import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { allNodes } from "src/shared/nodeType";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import { NodeEffect } from "src/shared/nodeEffect";
import * as Phaser from "phaser-ce";

export function verifyAndAddConnection(from: Node, to: Node, validFromNodes: number[], solution: Solution): ConnectResult {
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
    currentConnections.push({ to: to, distance: 0.0 });
  } else {
    solution[from.id] = [{ to: to, distance: 0.0 }];
  }

  return { tag: "ValidConnection", newValidFromNodes: validFromNodes, newSolution: solution };
}

type SuccessRunResult = {
  tag: "SuccessRunResult",
};

type FailRunResult = {
  tag: "FailRunResult",
};

type RunResult = SuccessRunResult | FailRunResult;

const startResources: RunResources = {
  "Basic": 0,
  "Red": 0,
  "Green": 0,
  "Blue": 0,
  "Yellow": 0,
  "Victory": 0,
};

export function runSolution(solution: Solution): RunResult {
  // TODO: get startNode from board?
  const startResult: StepResult = runStep({ id: 0, x: 0, y: 0, nodeType: allNodes.startNode }, solution, startResources);

  console.log("Run Result: " + JSON.stringify(startResult.resources));

  return { tag: "SuccessRunResult" };
}

type RunResources = {
  "Basic": number,
  "Red": number,
  "Green": number,
  "Blue": number,
  "Yellow": number,
  "Victory": number,
};

type EffectFunction = (resources: RunResources) => RunResources;

type StepResult = {
  resources: RunResources
};

function runStep(node: Node, solution: Solution, resources: RunResources): StepResult {
  console.log("Step: " + node.id);
  const connections: Connection[] | undefined = solution[node.id];
  if (connections === undefined) {
    // use final effect
    const func = effectFunction(node.nodeType.finalEffect);
    const newResources = func(resources);
    return { resources: newResources };
  } else {
    if (connections.length > 0) {
      // use link effect
      const nextNodes: Node[] = connections.map(conn => conn.to);

      let currentResources = resources;

      for (const nextNode of nextNodes) {
        const stepResult: StepResult = runStep(nextNode, solution, currentResources);
        currentResources = stepResult.resources;
      }

      return { resources: currentResources };
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
          newResources[gain.color] = newResources[gain.color] + gain.amount;
        }
        return newResources;
      };
    }
  }
}