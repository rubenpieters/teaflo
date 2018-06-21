import iassign from "immutable-assign";
import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { allNodes } from "src/shared/nodeType";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import { NodeEffect, triggerEffects } from "src/shared/rules/effect";
import { Modifier } from "src/shared/rules/modifier";
import { StepValues, emptyStepValues } from "src/shared/rules/resource";

function clamp(v: number, min: number, max: number) {
  return Math.min(Math.max(v, min), max);
}

function standardizeAngle(angle: number): number {
  let result: number = angle;
  while (result > 180) {
    result = result - 360;
  }
  while (result < -180) {
    result = result + 360;
  }
  return result;
}

export function verifyAndAddConnection(from: Node, to: Node, connectionId: number, validFromNodes: number[], solution: Solution): ConnectResult {
  // TODO: use Array.contains ?
  if (validFromNodes.filter(x => x === from.id).length < 1) {
    return { tag: "InvalidFromNode" };
  }

  /*if (from.nodeType.tag !== "StartNode") {
    const angleCenter: number = Math.atan2(from.y, from.x) * 180 / Math.PI;
    const angleNewLine: number = Math.atan2(to.y - from.y, to.x - from.x) * 180 / Math.PI;
    const verifyAngle = standardizeAngle(angleNewLine - angleCenter);
    console.log("ANGLE: " + verifyAngle);
    if (verifyAngle > 90 || verifyAngle < -90) {
      return { tag: "InvalidAngle" };
    }
  }*/

  validFromNodes.push(to.id);
  const currentConnections: Connection[] | undefined = solution[from.id];
  if (currentConnections !== undefined) {
    currentConnections.push({ from: from, to: to, distance: 0.0, connectionId: connectionId });
  } else {
    solution[from.id] = [{ from: from, to: to, distance: 0.0, connectionId: connectionId }];
  }

  return { tag: "ValidConnection", newValidFromNodes: validFromNodes, newSolution: solution };
}

type StepResult = {
  stepValues: StepValues,
  lastVisitedNodeId: number,
  validSolution: boolean,
};

export function initVisit(solution: Solution, limit: number): StepResult {
  const startNode = { id: 0, x: 0, y: 0, nodeType: allNodes.startNode, tier: 0 };
  return visitStep(solution, startNode, undefined, emptyStepValues(), [], limit, 0);
}

function visitStep(solution: Solution, node: Node, from: Node | undefined, stepValues: StepValues, nextConnections: Connection[], limit: number, count: number): StepResult {
  const visitResult = visitNode(solution, node, from, stepValues.modifiers);

  const newNextConnections: Connection[] = visitResult.next.concat(nextConnections);
  let stepValuesAfterGrowth: StepValues = stepValues;

  // update growth
  if (from !== undefined) {
    if (from.tier < node.tier ) {
      stepValuesAfterGrowth = iassign(stepValuesAfterGrowth,
        x => x.growth, x => x - (node.tier - from.tier));
    } else if (from.tier === node.tier) {
      stepValuesAfterGrowth = iassign(stepValuesAfterGrowth,
        x => x.growth, x => x - 1);
    }
  }

  let toTriggerEffects = visitResult.effects;
  if (from !== undefined && from.id === 0) {
    const clearTemp: NodeEffect[] = [{ tag: "ClearTemp" }];
    toTriggerEffects = clearTemp.concat(visitResult.effects);
  }

  const stepValuesAfterTrigger = triggerEffects(toTriggerEffects)(stepValuesAfterGrowth);
  if (stepValuesAfterTrigger === "Invalid") {
    return { stepValues: stepValues, lastVisitedNodeId: node.id, validSolution: false };
  } else {
    if (count + 1 >= limit) {
      return { stepValues: stepValuesAfterTrigger, lastVisitedNodeId: node.id, validSolution: true };
    } else if (newNextConnections.length === 0) {
      return { stepValues: stepValuesAfterTrigger, lastVisitedNodeId: node.id, validSolution: true };
    } else {
      const [nextConnection] = newNextConnections.splice(0, 1);
      return visitStep(solution, nextConnection.to, nextConnection.from, stepValuesAfterTrigger, newNextConnections, limit, count + 1);
    }
  }
}

function visitNode(solution: Solution, node: Node, from: Node | undefined, modifiers: Modifier[]): {
  next: Connection[],
  effects: NodeEffect[]
} {
  const connections: Connection[] | undefined = solution[node.id];

  if (connections === undefined) {
    const effects: NodeEffect[] = node.nodeType.effects;

    return { next: [], effects: effects };
  } else {
    if (connections.length > 0) {
      const effects: NodeEffect[] = node.nodeType.effects;

      return { next: connections, effects: effects };
    } else {
      console.log("Should not happen: " + node.id + " has empty connections");
      throw "Should not happen: " + node.id + " has empty connections";
    }
  }
}
