type LockStatus = "locked" | "unlocked";

export type SaveFileV1 = {
  version: "V1",
  actUnlocked: { [key: number]: LockStatus | undefined },
  levelUnlocked: { [key: string]: LockStatus | undefined },
}

export function actAvailable(
  saveFile: SaveFileV1,
  act: number,
) {
  return saveFile.actUnlocked[act] === "unlocked";
}

export function levelAvailable(
  saveFile: SaveFileV1,
  level: string,
) {
  return saveFile.levelUnlocked[level] === "unlocked";
}