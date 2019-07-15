import { GameRefs } from "../states/game";

export type Settings = {
  gameWidth: number,
  gameHeight: number,
  devMode: boolean,
}

export const initialSettings = {
  gameWidth: 1920,
  gameHeight: 1080,
  devMode: false,
};

export function saveSettings(
  gameRefs: GameRefs,
) {
  const settingsString = JSON.stringify(gameRefs.settings);
  localStorage.setItem("ca_saved_settings", settingsString);
}