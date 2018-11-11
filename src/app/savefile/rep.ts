type LockStatus = "locked" | "unlocked";

export type SaveFileV1 = {
  version: "V1",
  actUnlocked: { [key: number]: LockStatus | undefined },
  levelUnlocked: { [key: string]: LockStatus | undefined },
}