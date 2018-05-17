import { StepValues, emptyStepValues } from "src/shared/rules/resource";
import { NodeEffect, triggerEffects } from "src/shared/rules/effect";
import iassign from "immutable-assign";
import { Modifier } from "src/shared/rules/modifier";

function test1() {
  const gainEffect: NodeEffect = {
    tag: "GainEffect",
    gains: [
    { color: "Basic",
      type: "Temp",
      amount: 1,
    }]
  }
  
  const stepValues: StepValues = emptyStepValues();
  
  const newValues = triggerEffects([gainEffect])(stepValues);
  
  console.log(newValues);
}

function test2() {
  const gainEffect: NodeEffect = {
    tag: "GainEffect",
    gains: [
    { color: "Basic",
      type: "Temp",
      amount: 1,
    }]
  }

  const doubleGainMod: Modifier = {
    charges: 1,
    chargePerUse: 1,
    modifierEffect: {
      tag: "DoubleNextGain",
    }
  }

  const stepValues: StepValues = iassign(emptyStepValues(),
  x => x.modifiers, m => [doubleGainMod]);

  
  const newValues = triggerEffects([gainEffect])(stepValues);
  
  console.log(newValues);
  console.log(newValues.modifiers.map(x => JSON.stringify(x)));
}

test2();