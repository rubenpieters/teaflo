import { Board, emptyBoard } from "src/shared/board";

export type SelectedScreen = "Home" | "Play";

export type AppState = {
  selectedScreen: SelectedScreen,
  board: Board,
  branchedFrom?: Board
};

const appState: AppState = {
  selectedScreen: "Home",
  board: emptyBoard,
};

type ParamCallBack<A> = (a: A) => void;

const selectedScreenCallbacks: ParamCallBack<SelectedScreen>[] = [];
const boardCallBacks: ParamCallBack<Board>[] = [];

export function changeSelectedScreen(newScreen: SelectedScreen) {
  if (appState.selectedScreen !== newScreen) {
    appState.selectedScreen = newScreen;
    selectedScreenCallbacks.forEach((cb) => cb(newScreen));
  }
}

export function addSelectedScreenCallback(cb: ParamCallBack<SelectedScreen>) {
  selectedScreenCallbacks.push(cb);
}

export function changeBoard(board: Board) {
  appState.board = board;
  boardCallBacks.forEach(cb => cb(board));
}

export function addBoardCallBack(cb: ParamCallBack<Board>) {
  boardCallBacks.push(cb);
}