import { Action } from "src/shared/game/action";

export type Trigger = {
  onTag: string,
  type: "before", // | "after" | "on"
  action: Action,
}