import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { allNodes } from "src/shared/nodeType";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import { NodeEffect } from "src/shared/nodeEffect";
import { ResourceType, ResourceColor } from "src/shared/resourceType";
import { Modifier } from "src/shared/modifier";
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

const startStepData: () => StepData = function() {
  return {
    resources: startResources(),
    modifiers: [],
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

type EffectFunction = (stepData: StepData) => StepData;
type ModifierFunction = (effects: NodeEffect) => NodeEffect[];

type StepData = {
  resources: RunResources,
  modifiers: Modifier[],
};

type StepResult = {
  stepData: StepData,
  nodeId: number,
};

export function initVisit(solution: Solution, limit: number): StepResult {
  const startNode = { id: 0, x: 0, y: 0, nodeType: allNodes.startNode };
  return visitStep(solution, startNode, startStepData(), [], limit, 0);
}

function visitStep(solution: Solution, node: Node, stepData: StepData, nextNodes: Node[], limit: number, count: number): StepResult {
  const visitResult = visitNode(solution, node, stepData.modifiers);

  const newNextNodes = visitResult.next.map(conn => conn.to).concat(nextNodes);
  let newStepData = stepData;

  for (const effect of visitResult.effects) {
    newStepData = effectFunction(effect)(newStepData);
  }

  if (count + 1 >= limit) {
    return { stepData: newStepData, nodeId: node.id };
  } else if (newNextNodes.length === 0) {
    return { stepData: newStepData, nodeId: node.id };
  } else {
    const [nextNode] = newNextNodes.splice(0, 1);
    return visitStep(solution, nextNode, newStepData, newNextNodes, limit, count + 1);
  }
}

function visitNode(solution: Solution, node: Node, modifiers: Modifier[]) {
  const connections: Connection[] | undefined = solution[node.id];

  if (connections === undefined) {
    // use final effect
    const effect = node.nodeType.finalEffect;

    // apply modifiers
    let effects: NodeEffect[] = effect;
    for (const modifier of modifiers) {
      let effectsTmp: NodeEffect[] = [];
      for (const eff of effects) {
        effectsTmp = effectsTmp.concat(modifierFunction(modifier)(eff));
      }
      effects = effectsTmp;
    }

    return { next: [], effects: effects };
  } else {
    if (connections.length > 0) {
      // use link effect
      const effect = node.nodeType.linkEffect;

      // apply modifiers
      let effects: NodeEffect[] = effect;
      for (const modifier of modifiers) {
        let effectsTmp: NodeEffect[] = [];
        for (const eff of effects) {
          effectsTmp = effectsTmp.concat(modifierFunction(modifier)(eff));
        }
        effects = effectsTmp;
      }

      return { next: connections, effects: effects };
    } else {
      throw "Should not happen: " + node.id + " has empty connections";
    }
  }

}

export function effectFunction(effect: NodeEffect): EffectFunction {
  switch (effect.tag) {
    case "NilEffect": {
      return stepData => { return stepData; };
    }
    case "GainEffect": {
      return stepData => {
        const newResources: RunResources = Object.assign({}, stepData.resources);
        console.log("GAIN: " + effect.gains);
        for (const gain of effect.gains) {
          newResources[gain.color][gain.type] = newResources[gain.color][gain.type] + gain.amount;
        }
        return { resources: newResources, modifiers: stepData.modifiers };
      };
    }
    case "ClearTemp": {
      return stepData => {
        const newResources: RunResources = Object.assign({}, stepData.resources);
        for (const resourceColor of allColors) {
          newResources[resourceColor]["Temp"] = 0;
        }
        return { resources: newResources, modifiers: stepData.modifiers };
      };
    }
    case "ConsumeEffect": {
      return stepData => {
        if (checkResources(stepData.resources, effect.consume)) {
          let newResources: RunResources = Object.assign({}, stepData.resources);
          let newModifiers: Modifier[] = Object.assign({}, stepData.modifiers);
          payResources(newResources, effect.consume);
          for (const consumeEff of effect.afterConsume) {
            const newStepData = effectFunction(consumeEff)({ resources: newResources, modifiers: stepData.modifiers });
            newResources = newStepData.resources;
            newModifiers = newStepData.modifiers;
          }
          return { resources: newResources, modifiers: newModifiers };
        } else {
          return stepData;
        }
      };
    }
    case "PersistEffect": {
      return stepData => {
        let newResources: RunResources = Object.assign({}, stepData.resources);
        for (const color of allColors) {
          newResources[color]["Total"] += newResources[color]["Temp"];
          newResources[color]["Temp"] = 0;
        }
        return { resources: newResources, modifiers: stepData.modifiers };
      };
    }
    case "AddModifier": {
      return stepData => {
        const newModifiers: Modifier[] = stepData.modifiers.concat([effect.modifierType]);
        return { resources: stepData.resources, modifiers: newModifiers };
      };
    }
  }
}

function checkResources(resources: RunResources, toCheck: {
  color: ResourceColor,
  type: "Temp" | "Total" | "Both",
  amount: number,
}[]): boolean {
  for (const res of toCheck) {
    if (res.type === "Both") {
      const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
      if (amount < res.amount) {
        return false;
      }
    } else {
      if (resources[res.color][res.type] < res.amount) {
        return false;
      }
    }
  }
  return true;
}

function payResources(resources: RunResources, toPay: {
  color: ResourceColor,
  type: "Temp" | "Total" | "Both",
  amount: number,
}[]): boolean {
  for (const res of toPay) {
    if (res.type === "Both") {
      if (resources[res.color]["Temp"] >= res.amount) {
        resources[res.color]["Temp"] -= res.amount;
      } else {
        const toTakeFromTotal = res.amount - resources[res.color]["Total"];
        resources[res.color]["Temp"] = 0;
        resources[res.color]["Total"] -= toTakeFromTotal;
      }
      const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
      if (amount < res.amount) {
        return false;
      }
    } else {
      resources[res.color][res.type] -= res.amount;
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

function modifierFunction(modifier: Modifier): ModifierFunction {
  switch (modifier.effect.tag) {
    case "IgnoreNextConsume": {
      return effect => {
        switch (effect.tag) {
          case "ConsumeEffect": {
            return effect.afterConsume;
          }
          default: {
            return [effect];
          }
        }
      };
    }
    case "IgnoreNextCheck": {
      return effect => {
        // TODO
        return [effect];
      };
    }
  }
}