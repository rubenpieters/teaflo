import { ConnectionStatus } from "src/app/network/connectionStatus";

export type SelectedScreen = "Home" | "Play";

export type AppState = {
  selectedScreen: SelectedScreen,
  connectionStatus: ConnectionStatus,
};

const appState: AppState = {
  selectedScreen: "Home",
  connectionStatus: "notConnected",
};

type ParamCallBack<A> = (a: A) => void;

const selectedScreenCallbacks: ParamCallBack<SelectedScreen>[] = [];
const connectedCallbacks: ParamCallBack<ConnectionStatus>[] = [];

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

export function changeConnected(connectionStatus: ConnectionStatus) {
  appState.connectionStatus = connectionStatus;
  connectedCallbacks.forEach(cb => cb(connectionStatus));
}

export function addConnectedCallback(cb: ParamCallBack<ConnectionStatus>) {
  connectedCallbacks.push(cb);
}
