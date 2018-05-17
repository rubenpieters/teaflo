import { Modifier } from "src/shared/rules/modifier";

export type ResourceColor
  = "Basic"
  | "Victory"
  ;

export type ResourceType
  = "Temp"
  | "Total"
  ;

export type ResourceUnit = {
  color: ResourceColor
  type: ResourceType
  amount: number
};

export type ConsumeUnit = {
  color: ResourceColor,
  type: ResourceType | "Both",
  amount: number,
};

export type ResourceValues = {
  [K in ResourceColor]: {
    [K in ResourceType]: number
  }
};

export const emptyResourceValues: () => ResourceValues = () => {
  return {
    Basic: {
      Temp: 0,
      Total: 0,
    },
    Victory: {
      Temp: 0,
      Total: 0,
    },
  };
};

export type StepValues = {
  resources: ResourceValues,
  modifiers: Modifier[],
  growth: number,
};

export const emptyStepValues: () => StepValues = () => {
  return {
    resources: emptyResourceValues(),
    modifiers: [],
    growth: 0,
  };
};