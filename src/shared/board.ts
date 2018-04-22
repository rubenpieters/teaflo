import { NodeType } from "src/shared/nodeType";
import { Node } from "src/shared/node";

export type Board = Node[];

export const emptyBoard: Board = [];

export function generateBoard(rng: Rng, boardData: BoardData): Board {
  let result: Board = [];
  var nodeId: number = 0;
  const gst: GenerateState = { nextId: () => { const id = nodeId; nodeId += 1; return id; } };
  
  const startNode: Node = { id: 0, x: 0, y: 0, nodeType: { tag: "StartNode" } };
  result = [startNode];

  for (const level of boardData) {
    const sectionData: SectionData[] = generateSectionData(level.ampMin, level.ampMax, level.quadrants, level.levels);
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
  levels: number,
  amount: number
};

type BoardData = LevelData[];

export const boardData: BoardData = [
  {
    ampMin: 0,
    ampMax: 0,
    quadrants: [rng => { return { tag: "StartNode" }; }],
    levels: 1,
    amount: 1
  },
  {
    ampMin: 50,
    ampMax: 250,
    quadrants: [
      rng => { return { tag: "ResourceNode", cost: {}, gain: { basic: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { basic: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { basic: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { basic: 1 }}; },
    ],
    levels: 1,
    amount: 2,
  },
  {
    ampMin: 200,
    ampMax: 700,
    quadrants: [
      rng => { return { tag: "ResourceNode", cost: {}, gain: { blue: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { red: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { green: 1 }}; },
      rng => { return { tag: "ResourceNode", cost: {}, gain: { yellow: 1 }}; },
    ],
    levels: 1,
    amount: 2,
  },
  {
    ampMin: 600,
    ampMax: 800,
    quadrants: [
      rng => { return { tag: "VictoryNode", cost: {}}; },
      rng => { return { tag: "VictoryNode", cost: {}}; },
      rng => { return { tag: "VictoryNode", cost: {}}; },
      rng => { return { tag: "VictoryNode", cost: {}}; },
    ],
    levels: 1,
    amount: 2,
  },
  ];

type SectionData = {
  ampMin: number,
  ampMax: number,
  angleMin: number,
  angleMax: number,
  nodeTypes: Quadrant
};

function generateSectionData(
    ampMin: number, ampMax: number, quadrants: Quadrant[], levels: number
  ): SectionData[] {
  const result: SectionData[] = [];
  const quadrantSize: number = 360 / quadrants.length;
  for (let i = 0; i <= levels; i++) {
    const levelAmp: number = (ampMax - ampMin) / levels;
    const quadrantsWithIndex: { x: Quadrant, i: number}[] =
      quadrants.map((x, i) => { return { x: x, i: i }; });
    for (const { x: quadrant, i: index } of quadrantsWithIndex) {
      result.push({
        ampMin: ampMin + ((levels - 1) * levelAmp),
        ampMax: ampMin + (levels * levelAmp),
        angleMin: index * quadrantSize,
        angleMax: (index + 1) * quadrantSize,
        nodeTypes: quadrant
      });
    }
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
  for (let i = 1; i < amount; i++) {
    const r: number = rng.integerInRange(sectionData.ampMin, sectionData.ampMax)();
    const φ: number = rng.integerInRange(sectionData.angleMin, sectionData.angleMax)();
    const id: number = gst.nextId();
    const nodeType: NodeType = sectionData.nodeTypes(rng);
    result.push({
      id: id,
      x: r * Math.cos(φ),
      y: r * Math.sin(φ),
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
  const l: number = rng.integerInRange(0, array.length)();
  return array[l];
}


