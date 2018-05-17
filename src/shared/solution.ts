import { Board } from "src/shared/board";
import { Node } from "src/shared/node";
import { allNodes } from "src/shared/nodeType";
import { Connection, Solution, ConnectResult } from "src/shared/connectResult";
import { NodeEffect, triggerEffects } from "src/shared/rules/effect";
import { Modifier } from "src/shared/rules/modifier";
import * as Phaser from "phaser-ce";
import { StepValues, emptyStepValues } from "src/shared/rules/resource";

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

type StepResult = {
  stepValues: StepValues,
  nodeId: number,
};

export function initVisit(solution: Solution, limit: number): StepResult {
  const startNode = { id: 0, x: 0, y: 0, nodeType: allNodes.startNode, tier: 0 };
  return visitStep(solution, startNode, emptyStepValues(), [], limit, 0, undefined);
}

function visitStep(solution: Solution, node: Node, stepValues: StepValues, nextNodes: Node[], limit: number, count: number, prevTier: number | undefined): StepResult {
  const visitResult = visitNode(solution, node, stepValues.modifiers);

  const newNextNodes = visitResult.next.map(conn => conn.to).concat(nextNodes);
  let newStepValues: StepValues = stepValues;

  // update growth
  if (prevTier !== undefined) {
    newStepValues.growth -= node.tier - prevTier;
  }

  newStepValues = triggerEffects(visitResult.effects)(newStepValues);

  if (count + 1 >= limit) {
    return { stepValues: newStepValues, nodeId: node.id };
  } else if (newNextNodes.length === 0) {
    return { stepValues: newStepValues, nodeId: node.id };
  } else {
    const [nextNode] = newNextNodes.splice(0, 1);
    return visitStep(solution, nextNode, newStepValues, newNextNodes, limit, count + 1, visitResult.prevTier);
  }
}

function visitNode(solution: Solution, node: Node, modifiers: Modifier[]): {
  next: Connection[],
  effects: NodeEffect[],
  prevTier: number | undefined
} {
  const connections: Connection[] | undefined = solution[node.id];

  if (connections === undefined) {
    // use final effect
    const effects: NodeEffect[] = node.nodeType.finalEffect;

    return { next: [], effects: node.nodeType.finalEffect, prevTier: undefined };
  } else {
    if (connections.length > 0) {
      // use link effect
      const effects: NodeEffect[] = node.nodeType.linkEffect;

      return { next: connections, effects: effects, prevTier: node.tier };
    } else {
      throw "Should not happen: " + node.id + " has empty connections";
    }
  }
}