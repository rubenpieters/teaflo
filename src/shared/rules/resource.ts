import iassign from "immutable-assign";
import { Modifier } from "src/shared/rules/modifier";

export type ResourceColor
  = "Basic"
  | "Victory"
  | "Stack"
  ;

export const allColors: ResourceColor[] = ["Basic", "Victory", "Stack"];

export type ResourceType
  = "Temp"
  | "Total"
  ;

export type ResourceUnit = {
  color: ResourceColor | "Affinity"
  type: ResourceType
  amount: number
};

export type ConsumeUnit = {
  color: ResourceColor,
  type: ResourceType | "Both",
  amount: number,
};

export type ConvertUnit = {
  tag: "ConvertUnit",
  from: {
    color: ResourceColor,
    type: ResourceType,
  },
  to: {
    color: ResourceColor,
    type: ResourceType,
  },
  amount: number | "All",
};

export type ConvertBothUnit = {
  tag: "ConvertBothUnit",
  from: {
    color: ResourceColor,
    type: "Both",
  },
  to: {
    color: ResourceColor,
    type: ResourceType,
  },
  amount: number | "All",
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
    Stack: {
      Temp: 0,
      Total: 0,
    }
  };
};

export type StepValues = {
  resources: ResourceValues,
  modifiers: Modifier[],
  growth: number,
  affinity: ResourceColor,
};

export const emptyStepValues: () => StepValues = () => {
  return {
    resources: emptyResourceValues(),
    modifiers: [],
    growth: 0,
    affinity: "Basic",
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

export function convert(values: ResourceValues, convertUnit: ConvertUnit): ResourceValues {
  let toConvert: number = 0;
  if (typeof convertUnit.amount === "string") {
    toConvert = values[convertUnit.from.color][convertUnit.from.type];
  } else {
    toConvert = values[convertUnit.from.color][convertUnit.from.type] > convertUnit.amount ?
      convertUnit.amount : values[convertUnit.from.color][convertUnit.from.type];

  }
  values = iassign(iassign(values,
    x => x[convertUnit.to.color][convertUnit.to.type],
    x => x + toConvert),
    x => x[convertUnit.from.color][convertUnit.from.type],
    x => x - toConvert);
  return values;
}

export function convertFromBoth(values: ResourceValues, convertUnit: ConvertBothUnit): ResourceValues {
  let toConvertTemp: number = 0;
  let toConvertTotal: number = 0;
  if (typeof convertUnit.amount === "string") {
    toConvertTemp = values[convertUnit.from.color]["Temp"];
    toConvertTotal = values[convertUnit.from.color]["Total"];
  } else {
    toConvertTemp = values[convertUnit.from.color]["Temp"] > convertUnit.amount ?
      convertUnit.amount : values[convertUnit.from.color]["Temp"];
    toConvertTotal = values[convertUnit.from.color]["Total"] > convertUnit.amount - toConvertTemp ?
      convertUnit.amount - toConvertTemp : values[convertUnit.from.color]["Total"];
  }
  values = iassign(iassign(iassign(values,
    x => x[convertUnit.to.color][convertUnit.to.type],
    x => x + toConvertTemp + toConvertTotal),
    x => x[convertUnit.from.color]["Temp"],
    x => x - toConvertTemp),
    x => x[convertUnit.from.color]["Total"],
    x => x - toConvertTotal);
  return values;
}