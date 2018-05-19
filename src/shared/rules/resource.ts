import iassign from "immutable-assign";
import { Modifier } from "src/shared/rules/modifier";

export type ResourceColor
  = "Basic"
  | "Victory"
  ;

export const allColors: ResourceColor[] = ["Basic", "Victory"];

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

export type PersistUnit = {
  color: ResourceColor | "All",
  type: ResourceType,
  amount: number | "All",
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

export function persist(values: ResourceValues, color: ResourceColor, amount: number | "All"): ResourceValues {
  let toPersist: number = 0;
  if (typeof amount === "string") {
    toPersist = values[color]["Temp"];
  } else {
    toPersist = values[color]["Temp"] > amount ? amount : values[color]["Temp"];
  }
  values = iassign(iassign(values,
    x => x[color]["Total"],
    x => x + toPersist),
    x => x[color]["Temp"],
    x => x - toPersist);
  return values;
}