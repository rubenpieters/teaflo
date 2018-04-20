
export type SelectedScreen = "Home" | "Play";

export type AppState = {
  selectedScreen: SelectedScreen
};

const appState: AppState = {
  selectedScreen: "Home"
};

type SelectedScreenCallback = (screen: SelectedScreen) => void;

const selectedScreenCallbacks: SelectedScreenCallback[] = [];

export function changeSelectedScreen(newScreen: SelectedScreen) {
  if (appState.selectedScreen !== newScreen) {
    appState.selectedScreen = newScreen;
    console.log("new screen " + appState.selectedScreen);
    selectedScreenCallbacks.forEach((cb) => cb(newScreen));
  }
}

export function addSelectedScreenCallback(cb: SelectedScreenCallback) {
  selectedScreenCallbacks.push(cb);
}