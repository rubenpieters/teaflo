import { focus, over, set } from "src/shared/iassign-util";
import { ActionSpec } from "./action";

export type RequiresInput = {
  inputs: InputType[],
}

type TargetInput = {
  tag: "TargetInput",
};

type NumberInput = {
  tag: "NumberInput",
};

export type InputType
  = TargetInput
  | NumberInput
  ;

export type InputActionSpec = (inputs: any[]) => ActionSpec;