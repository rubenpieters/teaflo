import { Action } from "src/shared/game/action";
import { TargetSpec } from "src/shared/game/target";

export type Trigger = {
  onTag: string,
  type: "before", // | "after" | "on"
  action: Action<TargetSpec>,
}