import { NodeType } from "src/shared/nodeType";

export type GameState = {
  selectedNode: NodeType | undefined,
}

type ParamCallBack<A> = (a: A) => void;

const selectedNodeCallbacks: ParamCallBack<NodeType>[] = [];

const gameState: GameState = {
  selectedNode: undefined
}

export function changeSelectedNode(nodeType: NodeType) {
  gameState.selectedNode = nodeType;
  selectedNodeCallbacks.forEach(cb => cb(nodeType));
}

export function addNodeCallback(cb: ParamCallBack<NodeType>) {
  selectedNodeCallbacks.push(cb);
}