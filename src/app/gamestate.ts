import { NodeType } from "src/shared/nodeType";
import { Modifier } from "src/shared/modifier";
import { StepValues, emptyStepValues } from "src/shared/rules/resource";

export type GameState = {
  selectedNode: NodeType | undefined,
  shownResources: StepValues,
};

type ParamCallBack<A> = (a: A) => void;

const selectedNodeCallbacks: ParamCallBack<NodeType>[] = [];
const shownResourcesCallbacks: ParamCallBack<StepValues>[] = [];

const gameState: GameState = {
  selectedNode: undefined,
  shownResources: emptyStepValues(),
};

export function changeSelectedNode(nodeType: NodeType) {
  gameState.selectedNode = nodeType;
  selectedNodeCallbacks.forEach(cb => cb(nodeType));
}

export function addNodeCallback(cb: ParamCallBack<NodeType>) {
  selectedNodeCallbacks.push(cb);
}

export function changeShownResources(resources: StepValues) {
  gameState.shownResources = resources;
  shownResourcesCallbacks.forEach(cb => cb(resources));
}

export function addShownResourcesCallback(cb: ParamCallBack<StepValues>) {
  shownResourcesCallbacks.push(cb);
}
