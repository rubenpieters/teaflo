import { Solution } from "src/shared/connectResult"
import { allNodes } from "src/shared/nodeType"
import { initVisit } from "src/shared/solution"

function test1() {
  const sol: Solution = {
    0: [{
      to: {
        id: 1,
        x: 0,
        y: 0,
        tier: 0,
        nodeType: allNodes.resource1
      },
      from: {
        id: 1,
        x: 0,
        y: 0,
        tier: 0,
        nodeType: allNodes.resource2_2
      },
      distance: 1,
      connectionId: 0
    }],
    1: [{
      to: {
        id: 2,
        x: 0,
        y: 0,
        tier: 1,
        nodeType: allNodes.resource2_1
      },
      from: {
        id: 1,
        x: 0,
        y: 0,
        tier: 0,
        nodeType: allNodes.resource2_2
      },
      distance: 1,
      connectionId: 0
    }],
  }

  const result = initVisit(sol, 20);
  console.log(JSON.stringify(result));
}

function test2() {
  const sol: Solution = {
    0: [{
      to: {
        id: 1,
        x: 0,
        y: 0,
        tier: 1,
        nodeType: allNodes.resource2_2
      },
      from: {
        id: 1,
        x: 0,
        y: 0,
        tier: 0,
        nodeType: allNodes.resource2_2
      },
      distance: 1,
      connectionId: 0
    }],
  }

  const result = initVisit(sol, 20);
  console.log(JSON.stringify(result));
}

test2();