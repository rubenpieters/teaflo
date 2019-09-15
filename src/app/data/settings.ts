import { GameRefs } from "../states/game";

export type Settings = {
  gameWidth: number,
  gameHeight: number,
  devMode: boolean,
  version: string,
  versionVerbose: string,
  versionMessage: string,
}

export const initialSettings = {
  gameWidth: 1920,
  gameHeight: 1080,
  devMode: false,
  version: "0.0.1",
  versionVerbose: "prototype 1",
  versionMessage: "This version serves to showcase gameplay. Everything is subject to change and is not indicative of the final product.",
};

export function saveSettings(
  gameRefs: GameRefs,
) {
  const settingsString = JSON.stringify(gameRefs.settings);
  localStorage.setItem("ca_saved_settings", settingsString);
}