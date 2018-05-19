import iassign from "immutable-assign";
import { ResourceUnit, ConsumeUnit, PersistUnit, ResourceValues, ResourceColor, StepValues, allColors, persist } from "src/shared/rules/resource";
import { Modifier, modifierFunction } from "src/shared/rules/modifier";

type GainEffect = {
  tag: "GainEffect",
  gains: ResourceUnit[],
};

type ConsumeEffect = {
  tag: "ConsumeEffect",
  consume: ConsumeUnit[],
  afterConsume: NodeEffect[],
};

type CheckEffect = {
  tag: "CheckEffect",
  consume: ConsumeUnit[],
  afterCheck: NodeEffect[],
};

type ClearTemp = {
  tag: "ClearTemp",
};

type AddModifier = {
  tag: "AddModifier",
  modifier: Modifier,
};

type PersistEffect = {
  tag: "PersistEffect",
  persists: PersistUnit[],
};

export type NodeEffect
  = GainEffect
  | ConsumeEffect
  | CheckEffect
  | ClearTemp
  | AddModifier
  | PersistEffect
  ;

export function triggerEffects(nodeEffects: NodeEffect[]):
  (sv: StepValues) => StepValues {
  return stepValues => {
    let effects: NodeEffect[] = nodeEffects.concat();
    let returnValues: StepValues = stepValues;
    while (effects.length > 0) {
      // cast is safe since length > 0, effects.pop() can not be undefined
      const effect: NodeEffect = (<NodeEffect>effects.shift());
      const { newValues, newEffects } = triggerEffect(effect)(returnValues);
      returnValues = newValues;
      effects = newEffects.concat(effects);
    }
    return returnValues;
  };
}

export function triggerEffect(nodeEffect: NodeEffect):
  (sv: StepValues) => { newValues: StepValues, newEffects: NodeEffect[] } {
  return stepValues => {
    let effectAcc: NodeEffect | undefined = nodeEffect;
    let restEffectAcc: NodeEffect[] = [];
    let modifierAcc: Modifier[] = [];
    for (const modifier of stepValues.modifiers) {
      if (effectAcc === undefined) {
        return { newValues: stepValues, newEffects: restEffectAcc };
      } else {
        const { newEffects, newModifiers } = modifierFunction(modifier)(effectAcc);
        if (newEffects.length > 0) {
          effectAcc = newEffects[0];
          restEffectAcc = restEffectAcc.concat(newEffects.slice(1));
        } else {
          effectAcc = undefined;
        }
        modifierAcc = modifierAcc.concat(newModifiers);
      }
    }
    const valuesAfterModifier: StepValues  = iassign(stepValues,
      v => v.modifiers, m => modifierAcc);
    if (effectAcc === undefined) {
      return { newValues: stepValues, newEffects: restEffectAcc };
    } else {
      return effectFunction(effectAcc)(valuesAfterModifier);
    }
  };
}

export function effectFunction(effect: NodeEffect):
  (sv: StepValues) => { newValues: StepValues, newEffects: NodeEffect[] } {
  return stepValues => {
    switch (effect.tag) {
      case "GainEffect": {
        let newStepValues: StepValues = stepValues;
        for (const gain of effect.gains) {
          newStepValues = iassign(newStepValues,
            v => v.resources[gain.color][gain.type], x => x + gain.amount);
        }
        return { newValues: newStepValues, newEffects: [] };
      }
      case "ConsumeEffect": {
        let newStepValues: StepValues = stepValues;

        const payResult = payResources(stepValues.resources, effect.consume);
        if (typeof payResult === "string") {
          // TODO: return Either
          console.log("can not pay consume");
          throw "can not pay consume";
        } else {
          newStepValues = iassign(newStepValues, v => v.resources, x => payResult);
          return { newValues: newStepValues, newEffects: effect.afterConsume };
        }
      }
      case "CheckEffect": {
        const payResult = payResources(stepValues.resources, effect.consume);
        if (typeof payResult === "string") {
          return { newValues: stepValues, newEffects: [] };
        } else {
          return { newValues: stepValues, newEffects: effect.afterCheck };
        }
      }
      case "ClearTemp": {
        let newStepValues: StepValues = stepValues;
        for (const color of allColors) {
          newStepValues = iassign(newStepValues, x => x.resources[color]["Temp"], x => 0);
        }
        return { newValues: newStepValues, newEffects: [] };
      }
      case "AddModifier": {
        const newStepValues: StepValues = iassign(stepValues,
          x => x.modifiers, x => x.concat([effect.modifier]));
        return { newValues: newStepValues, newEffects: [] };
      }
      case "PersistEffect": {
        let newStepValues: StepValues = stepValues;
        for (const persistUnit of effect.persists) {
          if (persistUnit.color !== "All") {
            // safe cast due to to if-stmt
            const color = <ResourceColor>persistUnit.color;
            newStepValues = iassign(newStepValues,
              x => x.resources, x => persist(x, color, persistUnit.amount));
          } else {
            for (const color of allColors) {
              newStepValues = iassign(newStepValues,
                x => x.resources, x => persist(x, color, persistUnit.amount));
            }
          }
        }
        return { newValues: newStepValues, newEffects: [] };
      }
    }
  };
}

function payResources(resources: ResourceValues, toPay: ConsumeUnit[]): ResourceValues | "NotEnough" {
  let newResources: ResourceValues = resources;
  for (const res of toPay) {
    const payResult = payResource(newResources, res);
    if (typeof payResult === "string") {
      return "NotEnough";
    } else {
      newResources = payResult;
    }
  }
  return newResources;
}

function payResource(resources: ResourceValues, res: ConsumeUnit): ResourceValues | "NotEnough" {
  if (res.type === "Both") {
    const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
    if (amount < res.amount) {
      return "NotEnough";
    }
    if (resources[res.color]["Temp"] >= res.amount) {
      const newResources: ResourceValues = iassign(resources,
        x => x[res.color]["Temp"], x => x - res.amount);
      return newResources;
    } else {
      const toTakeFromTotal = res.amount - resources[res.color]["Total"];
      const newResources: ResourceValues = iassign(iassign(resources,
        x => x[res.color]["Temp"], x => 0),
        x => x[res.color]["Total"], x => x - toTakeFromTotal);
      return newResources;
    }
  } else {
    const newResources: ResourceValues = iassign(resources,
      // cast safe due to if statement
      // TODO: no cast needed with switch?
      x => x[res.color][<"Temp" | "Total">res.type], x => x - res.amount);
    return newResources;
  }
}

/*
resource
-consume x: gain x
-check x: gain x
util
-ignore next consume
-add x to next gain
-persist x
score
-convert x to vp

*/

/*


export function effectFunction(effect: NodeEffect): EffectFunction {
  switch (effect.tag) {
    case "NilEffect": {
      return stepData => { return stepData; };
    }
    case "GainEffect": {
      return stepData => {
        const newResources: RunResources = Object.assign({}, stepData.resources);
        for (const gain of effect.gains) {
          newResources[gain.color][gain.type] = newResources[gain.color][gain.type] + gain.amount;
        }
        return { resources: newResources, modifiers: stepData.modifiers, growth: stepData.growth };
      };
    }
    case "ClearTemp": {
      return stepData => {
        const newResources: RunResources = Object.assign({}, stepData.resources);
        for (const resourceColor of allColors) {
          newResources[resourceColor]["Temp"] = 0;
        }
        return { resources: newResources, modifiers: stepData.modifiers, growth: stepData.growth };
      };
    }
    case "ConsumeEffect": {
      return stepData => {
        if (checkResources(stepData.resources, effect.consume)) {
          let newResources: RunResources = Object.assign({}, stepData.resources);
          let newModifiers: Modifier[] = stepData.modifiers.slice();
          let newGrowth: number = stepData.growth;
          payResources(newResources, effect.consume);
          for (const consumeEff of effect.afterConsume) {
            const newStepData = effectFunction(consumeEff)({ resources: newResources, modifiers: newModifiers, growth: newGrowth });
            newResources = newStepData.resources;
            newModifiers = newStepData.modifiers;
            newGrowth = newStepData.growth;
          }
          return { resources: newResources, modifiers: newModifiers, growth: stepData.growth };
        } else {
          return stepData;
        }
      };
    }
    case "PersistEffect": {
      return stepData => {
        let newResources: RunResources = Object.assign({}, stepData.resources);
        for (const color of allColors) {
          newResources[color]["Total"] += newResources[color]["Temp"];
          newResources[color]["Temp"] = 0;
        }
        return { resources: newResources, modifiers: stepData.modifiers, growth: stepData.growth };
      };
    }
    case "AddModifier": {
      return stepData => {
        const newModifiers: Modifier[] = stepData.modifiers.concat([effect.modifierType]);
        return { resources: stepData.resources, modifiers: newModifiers, growth: stepData.growth };
      };
    }
  }
}

function checkResources(resources: RunResources, toCheck: {
  color: ResourceColor,
  type: "Temp" | "Total" | "Both",
  amount: number,
}[]): boolean {
  for (const res of toCheck) {
    if (res.type === "Both") {
      const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
      if (amount < res.amount) {
        return false;
      }
    } else {
      if (resources[res.color][res.type] < res.amount) {
        return false;
      }
    }
  }
  return true;
}

function payResources(resources: RunResources, toPay: {
  color: ResourceColor,
  type: "Temp" | "Total" | "Both",
  amount: number,
}[]): boolean {
  for (const res of toPay) {
    if (res.type === "Both") {
      if (resources[res.color]["Temp"] >= res.amount) {
        resources[res.color]["Temp"] -= res.amount;
      } else {
        const toTakeFromTotal = res.amount - resources[res.color]["Total"];
        resources[res.color]["Temp"] = 0;
        resources[res.color]["Total"] -= toTakeFromTotal;
      }
      const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
      if (amount < res.amount) {
        return false;
      }
    } else {
      resources[res.color][res.type] -= res.amount;
    }
  }
  return true;
}

const allColors: ResourceColor[] = ["Basic", "Red", "Green", "Blue", "Yellow", "Victory"];

function clearResourceTypes(types: ResourceType[], resources: RunResources): void {
  for (const resourceType of types) {
    for (const resourceColor of allColors) {
      resources[resourceColor][resourceType] = 0;
    }
  }
}

function modifierFunction(modifier: Modifier): ModifierFunction {
  switch (modifier.effect.tag) {
    case "IgnoreNextConsume": {
      return effect => {
        switch (effect.tag) {
          case "ConsumeEffect": {
            return effect.afterConsume;
          }
          default: {
            return [effect];
          }
        }
      };
    }
    case "IgnoreNextCheck": {
      return effect => {
        // TODO
        return [effect];
      };
    }
  }
}
*/