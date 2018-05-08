import { NodeType } from "src/shared/nodeType";
import { Modifier } from "src/shared/modifier";

export type GameState = {
  selectedNode: NodeType | undefined,
  shownResources: StepData,
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

type StepData = {
  resources: RunResources,
  modifiers: Modifier[],
  growth: number,
};

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
    growth: 15,
  };
};

type ParamCallBack<A> = (a: A) => void;

const selectedNodeCallbacks: ParamCallBack<NodeType>[] = [];
const shownResourcesCallbacks: ParamCallBack<StepData>[] = [];

const gameState: GameState = {
  selectedNode: undefined,
  shownResources: startStepData(),
};

export function changeSelectedNode(nodeType: NodeType) {
  gameState.selectedNode = nodeType;
  selectedNodeCallbacks.forEach(cb => cb(nodeType));
}

export function addNodeCallback(cb: ParamCallBack<NodeType>) {
  selectedNodeCallbacks.push(cb);
}

export function changeShownResources(resources: StepData) {
  gameState.shownResources = resources;
  shownResourcesCallbacks.forEach(cb => cb(resources));
}

export function addShownResourcesCallback(cb: ParamCallBack<StepData>) {
  shownResourcesCallbacks.push(cb);
}
