import { NodeType } from "src/shared/nodeType";
import { Modifier } from "src/shared/modifier";
import { StepValues, emptyStepValues } from "src/shared/rules/resource";

export type GameState = {
  selectedNode: NodeType | undefined,
  shownResources: { values: StepValues, valid: boolean },
};

type ParamCallBack<A> = (a: A) => void;

const selectedNodeCallbacks: ParamCallBack<NodeType>[] = [];
const shownResourcesCallbacks: ParamCallBack<{ values: StepValues, valid: boolean }>[] = [];

const gameState: GameState = {
  selectedNode: undefined,
  shownResources: { values: emptyStepValues(), valid: true },
};

export function changeSelectedNode(nodeType: NodeType) {
  gameState.selectedNode = nodeType;
  selectedNodeCallbacks.forEach(cb => cb(nodeType));
}

export function addNodeCallback(cb: ParamCallBack<NodeType>) {
  selectedNodeCallbacks.push(cb);
}

export function changeShownResources(resources: {values: StepValues, valid: boolean}) {
  gameState.shownResources = resources;
  shownResourcesCallbacks.forEach(cb => cb(resources));
}

export function addShownResourcesCallback(cb: ParamCallBack<{ values: StepValues, valid: boolean }>) {
  shownResourcesCallbacks.push(cb);
}
