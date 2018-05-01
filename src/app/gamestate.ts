import { NodeType } from "src/shared/nodeType";

export type GameState = {
  selectedNode: NodeType | undefined,
  shownResources: RunResources,
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

type ParamCallBack<A> = (a: A) => void;

const selectedNodeCallbacks: ParamCallBack<NodeType>[] = [];
const shownResourcesCallbacks: ParamCallBack<RunResources>[] = [];

const gameState: GameState = {
  selectedNode: undefined,
  shownResources: startResources(),
};

export function changeSelectedNode(nodeType: NodeType) {
  gameState.selectedNode = nodeType;
  selectedNodeCallbacks.forEach(cb => cb(nodeType));
}

export function addNodeCallback(cb: ParamCallBack<NodeType>) {
  selectedNodeCallbacks.push(cb);
}

export function changeShownResources(resources: RunResources) {
  gameState.shownResources = resources;
  shownResourcesCallbacks.forEach(cb => cb(resources));
}

export function addShownResourcesCallback(cb: ParamCallBack<RunResources>) {
  shownResourcesCallbacks.push(cb);
}
