import iassign from "immutable-assign";
import { ResourceUnit, ConsumeUnit, PersistUnit, ConvertUnit, ConvertBothUnit, ResourceValues, ResourceColor, StepValues, allColors, persist, convert, convertFromBoth } from "src/shared/rules/resource";
import { Modifier, showModifier, modifierFunction, loseCharge, refreshCharge } from "src/shared/rules/modifier";

type GainEffect = {
  tag: "GainEffect",
  gain: ResourceUnit,
};

type LoseEffect = {
  tag: "LoseEffect",
  loss: ConsumeUnit,
};

type ConsumeEffect = {
  tag: "ConsumeEffect",
  consume: ConsumeUnit[],
  afterConsume: NodeEffect[],
};

type CheckEffect = {
  tag: "CheckEffect",
  check: ConsumeUnit[],
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

type ConvertEffect = {
  tag: "ConvertEffect",
  converts: ConvertUnit | ConvertBothUnit,
};

type LoseChargeEffect = {
  tag: "LoseChargeEffect",
};

type GainChargeEffect = {
  tag: "GainChargeEffect",
  value: 1,
};

type RefreshChargeEffect = {
  tag: "RefreshChargeEffect",
};

export type NodeEffect
  = GainEffect
  | LoseEffect
  | ConsumeEffect
  | CheckEffect
  | ClearTemp
  | AddModifier
  | PersistEffect
  | ConvertEffect
  | LoseChargeEffect
  | GainChargeEffect
  | RefreshChargeEffect
  ;

export function showEffect(nodeEffect: NodeEffect): string {
  switch (nodeEffect.tag) {
    case "GainEffect": {
      return "Gain " + nodeEffect.gain.amount + " " + nodeEffect.gain.color + " " + nodeEffect.gain.type;
    }
    case "LoseEffect": {
      return "Lose " + nodeEffect.loss.amount + " " + nodeEffect.loss.color + " " + nodeEffect.loss.type;
    }
    case "ConsumeEffect": {
      return "Consume" + "\n  " +
        nodeEffect.afterConsume.map(showEffect).join("  \n");
    }
    case "CheckEffect": {
      return "Consume " + "  \n" +
        nodeEffect.afterCheck.map(showEffect).join("  \n");
    }
    case "ClearTemp": {
      return "ClearTemp";
    }
    case "AddModifier": {
      return "AddMod: " + showModifier(nodeEffect.modifier);
    }
    case "PersistEffect": {
      return "Persist";
    }
    case "ConvertEffect": {
      return "Convert " + nodeEffect.converts.from.color + " " + nodeEffect.converts.from.type +
        " to " + nodeEffect.converts.to.color + " " + nodeEffect.converts.to.type +
        "(" + nodeEffect.converts.amount + ")";
    }
    case "LoseChargeEffect": {
      return "LoseChargeEffect";
    }
    case "GainChargeEffect": {
      return "Each Mod Gain " + nodeEffect.value + " Charges";
    }
    case "RefreshChargeEffect": {
      return "RefreshChargeEffect";
    }
  }
}

export function triggerEffects(nodeEffects: NodeEffect[]):
  (sv: StepValues) => StepValues {
  return stepValues => {
    let effects: NodeEffect[] = nodeEffects.concat();
    let returnValues: StepValues = stepValues;
    while (effects.length > 0) {
      // cast is safe since length > 0, effects.pop() can not be undefined
      const effect: NodeEffect = (<NodeEffect>effects.shift());
      const { newValues, newEffects } = triggerEffect(effect)(returnValues);

      const stackValue: number = newValues.resources.Stack.Temp + newValues.resources.Stack.Total;
      if (stackValue < newValues.modifiers.length) {
        // remove modifiers, if stack resource is too low
        returnValues = iassign(newValues, x => x.modifiers, x => x.slice(0, stackValue));
      } else {
        returnValues = newValues;
      }
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
        const newStepValues: StepValues = iassign(stepValues,
          v => v.resources[effect.gain.color][effect.gain.type], x => x + effect.gain.amount);
        return { newValues: newStepValues, newEffects: [] };
      }
      case "LoseEffect": {
        const newStepValues: StepValues = iassign(stepValues,
          // not sure why this typechecks, paying can fail with "NotEnough"
          v => v.resources, x => payResource(x, effect.loss));
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
        const payResult = payResources(stepValues.resources, effect.check);
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
        if (stepValues.resources.Stack.Temp + stepValues.resources.Stack.Total > stepValues.modifiers.length) {
          const newStepValues: StepValues = iassign(stepValues,
            x => x.modifiers, x => x.concat([effect.modifier]));
          return { newValues: newStepValues, newEffects: [] };
        } else {
          console.log("Stack FULL!");
          return { newValues: stepValues, newEffects: [] };
        }
      }
      case "PersistEffect": {
        let newStepValues: StepValues = stepValues;
        for (const persistUnit of effect.persists) {
          if (persistUnit.color !== "All") {
            const color: ResourceColor = persistUnit.color;
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
      case "ConvertEffect": {
        let newStepValues: StepValues = stepValues;
        switch (effect.converts.tag) {
          case "ConvertUnit": {
            const convertUnit: ConvertUnit = effect.converts;
            newStepValues = iassign(newStepValues,
              x => x.resources, x => convert(x, convertUnit));
            break;
          }
          case "ConvertBothUnit": {
            const convertUnit: ConvertBothUnit = effect.converts;
            newStepValues = iassign(newStepValues,
              x => x.resources, x => convertFromBoth(x, convertUnit));
              break;
          }
        }
        return { newValues: newStepValues, newEffects: [] };
      }
      case "LoseChargeEffect": {
        // TODO: find a typesafe way to filter?
        const newModifiers = <Modifier[]>stepValues.modifiers.map(m => loseCharge(m)).filter(x => x !== undefined);
        const newStepValues: StepValues = iassign(stepValues,
          x => x.modifiers, x => newModifiers);
        return { newValues: newStepValues, newEffects: [] };
      }
      case "GainChargeEffect": {
        const newModifiers = stepValues.modifiers.map(m => iassign(iassign(m,
          x => x.charges, x => x + effect.value),
          x => x.maxCharges, x => x + effect.value)
        );
        const newStepValues: StepValues = iassign(stepValues,
          x => x.modifiers, x => newModifiers);
        return { newValues: newStepValues, newEffects: [] };
      }
      case "RefreshChargeEffect": {
        const newModifiers = stepValues.modifiers.map(m => refreshCharge(m));
        const newStepValues: StepValues = iassign(stepValues,
          x => x.modifiers, x => newModifiers);
        return { newValues: newStepValues, newEffects: [] };
      }
    }
  };
}

function payResources(resources: ResourceValues, toPay: ConsumeUnit[]): ResourceValues | "NotEnough" {
  console.log(JSON.stringify(resources));
  let newResources: ResourceValues = resources;
  for (const res of toPay) {
    const payResult = payResource(newResources, res);
    if (typeof payResult === "string") {
      return "NotEnough";
    } else {
      newResources = payResult;
    }
  }
  console.log(JSON.stringify(newResources));
  return newResources;
}

function payResource(resources: ResourceValues, res: ConsumeUnit): ResourceValues | "NotEnough" {
  if (res.type !== "Both") {
    const newResources: ResourceValues = iassign(resources,
      // cast safe due to if statement
      x => x[res.color][<"Temp" | "Total">res.type], x => x - res.amount);
    return newResources;
  } else {
    const amount: number = resources[res.color]["Temp"] + resources[res.color]["Total"];
    if (amount < res.amount) {
      return "NotEnough";
    }
    if (resources[res.color]["Temp"] >= res.amount) {
      const newResources: ResourceValues = iassign(resources,
        x => x[res.color]["Temp"], x => x - res.amount);
      return newResources;
    } else {
      const toTakeFromTotal = res.amount - resources[res.color]["Temp"];
      const newResources: ResourceValues = iassign(iassign(resources,
        x => x[res.color]["Temp"], x => 0),
        x => x[res.color]["Total"], x => x - toTakeFromTotal);
      return newResources;
    }
  }
}