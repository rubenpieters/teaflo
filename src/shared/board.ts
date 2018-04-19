import { NodeType } from "src/shared/nodeType"

type Node =
  { id: number,
    x: number,
    y: number,
    nodeType: NodeType
  };

type Board = Node[];

function generateBoard(rng: Rng, boardData: BoardData): Board {
  var result: Board = [];
  const startNode = { id: 0, x: 0, y: 0, nodeType: { tag: "StartNode" } }
  for (let level of boardData) {
    const sectionData: SectionData[] = generateSectionData(level.ampMin, level.ampMax, level.quadrants, level.levels);
    for (let sectionDatum of sectionData) {
      const nodes: Node[] = generateSection(rng, sectionDatum, level.amount);
      result.concat(nodes);
    }
  }
  return result;
};

type Quadrant = (rng: Rng) => NodeType;

type LevelData =
  { ampMin: number,
    ampMax: number,
    quadrants: Quadrant[],
    levels: number,
    amount: number
  };

type BoardData = LevelData[];

const boardData: BoardData = [
  {
    ampMin: 0,
    ampMax: 0,
    quadrants: [(rng) => { return { tag: "StartNode" }}],
    levels: 1,
    amount: 1
  }/*,
  {
    ampMin: 50,
    ampMax: 250,
    quadrants: [
      
    ]
  }*/
  ]

type SectionData =
  { ampMin: number,
    ampMax: number,
    angleMin: number,
    angleMax: number,
    nodeTypes: Quadrant
  }

function generateSectionData(
    ampMin: number, ampMax: number, quadrants: Quadrant[], levels: number
  ): SectionData[] {
  var result: SectionData[] = [];
  const quadrantSize: number = 360 / quadrants.length;
  for (let i = 0; i <= levels; i++) {
    const levelAmp: number = (ampMax - ampMin) / levels;
    const quadrantsWithIndex: { x: Quadrant, i: number}[] =
      quadrants.map((x, i) => { return { x: x, i: i }; });
    for (let { x: quadrant, i: index } of quadrantsWithIndex) {
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
  nextId: () => number,
  integerInRange: (n1:number, n2:number) => () => number
}

function generateSection(rng: Rng, sectionData: SectionData, amount: number): Node[] {
  var result: Node[] = [];
  for (let i = 1; i < amount; i++) {
    const r: number = rng.integerInRange(sectionData.ampMin, sectionData.ampMax)();
    const φ: number = rng.integerInRange(sectionData.angleMin, sectionData.angleMax)();
    const id: number = rng.nextId();
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
    case "red": return ["green", "yellow", "blue"]
    case "green": return ["red", "yellow", "blue"]
    case "yellow": return ["green", "red", "blue"]
    case "blue": return ["green", "yellow", "red"]
  }
}

function chooseSet<A>(rng: Rng, array: A[]): A {
  const l: number = rng.integerInRange(0, array.length)();
  return array[l];
}


