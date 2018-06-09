import { Board, emptyBoard } from "src/shared/board";
import { ConnectionStatus } from "src/app/network/connectionStatus";

export type SelectedScreen = "Home" | "Play";

export type AppState = {
  selectedScreen: SelectedScreen,
  board: Board,
  branchedFrom?: Board,
  connectionStatus: ConnectionStatus,
  currentFilter: string | undefined,
};

const appState: AppState = {
  selectedScreen: "Home",
  board: emptyBoard,
  connectionStatus: "notConnected",
  currentFilter: undefined,
};

type ParamCallBack<A> = (a: A) => void;

const selectedScreenCallbacks: ParamCallBack<SelectedScreen>[] = [];
const boardCallbacks: ParamCallBack<Board>[] = [];
const connectedCallbacks: ParamCallBack<ConnectionStatus>[] = [];
const currentFilterCallbacks: ParamCallBack<{ board: Board, filter: string | undefined }>[] = [];

export function changeSelectedScreen(newScreen: SelectedScreen) {
  if (appState.selectedScreen !== newScreen) {
    appState.selectedScreen = newScreen;
    selectedScreenCallbacks.forEach((cb) => cb(newScreen));
  }
}

export function addSelectedScreenCallback(cb: ParamCallBack<SelectedScreen>) {
  selectedScreenCallbacks.push(cb);
}

export function getSelectedScreen(): SelectedScreen {
  return appState.selectedScreen;
}

export function changeBoard(board: Board) {
  // TODO clear old state from old board
  appState.board = board;
  boardCallbacks.forEach(cb => cb(board));
}

export function nodeLocation(nodeId: number) {
  return { x: appState.board[nodeId].x, y: appState.board[nodeId].y };
}

export function addBoardCallback(cb: ParamCallBack<Board>) {
  boardCallbacks.push(cb);
}

export function changeConnected(connectionStatus: ConnectionStatus) {
  appState.connectionStatus = connectionStatus;
  connectedCallbacks.forEach(cb => cb(connectionStatus));
}

export function addConnectedCallback(cb: ParamCallBack<ConnectionStatus>) {
  connectedCallbacks.push(cb);
}

export function changeCurrentFilter(filter: string | undefined) {
  appState.currentFilter = filter;
  currentFilterCallbacks.forEach(cb => cb({ board: appState.board, filter: filter }));
}

export function addCurrentFilterCallback(cb: ParamCallBack<{ board: Board, filter: string | undefined }>) {
  currentFilterCallbacks.push(cb);
}
