import { allNodes, NodeType } from "src/shared/nodeType";
import { Node } from "src/shared/node";

export type Board = Node[];

export const emptyBoard: Board = [];

export function generateBoard(rng: Rng, boardData: BoardData): Board {
  let result: Board = [];
  let nodeId: number = 1;
  const gst: GenerateState = { nextId: () => { const id = nodeId; nodeId += 1; return id; } };

  result = [{ id: 0, x: 0, y: 0, nodeType: allNodes.startNode }];

  for (const level of boardData) {
    const sectionData: SectionData[] = generateSectionData(level.ampMin, level.ampMax, level.quadrants);
    for (const sectionDatum of sectionData) {
      const nodes: Node[] = generateSection(rng, gst, sectionDatum, level.amount);
      result = result.concat(nodes);
    }
  }
  return result;
}

type Quadrant = (rng: Rng) => NodeType;

type LevelData = {
  ampMin: number,
  ampMax: number,
  quadrants: Quadrant[],
  amount: number,
};

type BoardData = LevelData[];

function chooseT1(rng: Rng): NodeType {
  return chooseSet(rng, [allNodes.twoBasicBranch, allNodes.oneBasicFork]);
}

export const boardData: BoardData = [
  {
    ampMin: 50,
    ampMax: 100,
    quadrants: [
      rng => { return allNodes.twoRedTemp; },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
    ],
    amount: 2,
  },
  {
    ampMin: 150,
    ampMax: 200,
    quadrants: [
      rng => { return allNodes.consume1RTo3R; },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
    ],
    amount: 3,
  },
  {
    ampMin: 225,
    ampMax: 250,
    quadrants: [
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
      rng => { return chooseT1(rng); },
    ],
    amount: 2,
  }
  ];

type SectionData = {
  ampMin: number,
  ampMax: number,
  angleMin: number,
  angleMax: number,
  nodeTypes: Quadrant
};

function generateSectionData(
    ampMin: number, ampMax: number, quadrants: Quadrant[]
  ): SectionData[] {
  const result: SectionData[] = [];
  const quadrantSize: number = 360 / quadrants.length;
  const quadrantsWithIndex: { x: Quadrant, i: number}[] =
    quadrants.map((x, i) => { return { x: x, i: i }; });
  for (const { x: quadrant, i: index } of quadrantsWithIndex) {
    result.push({
      ampMin: ampMin,
      ampMax: ampMax,
      angleMin: index * quadrantSize,
      angleMax: (index + 1) * quadrantSize,
      nodeTypes: quadrant
    });
  }

  return result;
}

type Rng = {
  integerInRange: (n1: number, n2: number) => () => number
};

type GenerateState = {
  nextId: () => number
};

function generateSection(rng: Rng, gst: GenerateState, sectionData: SectionData, amount: number): Node[] {
  const result: Node[] = [];
  for (let i = 0; i < amount; i++) {
    const r: number = rng.integerInRange(sectionData.ampMin, sectionData.ampMax)();
    const phi: number = sectionData.angleMin + (sectionData.angleMax - sectionData.angleMin) * (i + 0.5) / amount;
    const phiRadians: number = phi * Math.PI / 180;
    const id: number = gst.nextId();
    const nodeType: NodeType = sectionData.nodeTypes(rng);
    result.push({
      id: id,
      x: r * Math.cos(phiRadians),
      y: r * Math.sin(phiRadians),
      nodeType: nodeType
    });
  }
  return result;
}

type Color = "red" | "green" | "yellow" | "blue";

function oppositeColor(color: Color): Color[] {
  switch (color) {
    case "red": return ["green", "yellow", "blue"];
    case "green": return ["red", "yellow", "blue"];
    case "yellow": return ["green", "red", "blue"];
    case "blue": return ["green", "yellow", "red"];
  }
}

function chooseSet<A>(rng: Rng, array: A[]): A {
  const l: number = rng.integerInRange(0, array.length - 1)();
  return array[l];
}


