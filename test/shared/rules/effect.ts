import { StepValues, emptyStepValues } from "src/shared/rules/resource";
import { NodeEffect, triggerEffects } from "src/shared/rules/effect";
import iassign from "immutable-assign";
import { Modifier } from "src/shared/rules/modifier";

function test1() {
  const gainEffect: NodeEffect = {
    tag: "GainEffect",
    gain: 
      { color: "Basic",
        type: "Temp",
        amount: 1,
      }
  }

  const stepValues: StepValues = emptyStepValues();
  
  const newValues = triggerEffects([gainEffect])(stepValues);
  
  console.log(newValues);
}

function test2() {
  const gainEffect: NodeEffect = {
    tag: "GainEffect",
    gain: 
      { color: "Basic",
        type: "Temp",
        amount: 1,
      }
  }

  const doubleGainMod: Modifier = {
    charges: 1,
    chargePerUse: 1,
    maxCharges: 1,
    fragile: false,
    modifierEffect: {
      tag: "DoubleNextGain",
    }
  }

  const stepValues: StepValues = iassign(emptyStepValues(),
    x => x.modifiers, m => [doubleGainMod]);

  
  const newValues = triggerEffects([gainEffect])(stepValues);
  
  console.log(JSON.stringify(newValues));
}

function test3() {
  const consumeEffect: NodeEffect = {
    tag: "ConsumeEffect",
    consume: [
      { color: "Basic", type: "Both", amount: 1 }
    ],
    afterConsume: [
      {
        tag: "GainEffect",
        gain: 
          { color: "Basic",
            type: "Temp",
            amount: 3,
          }
      }
    ]
  }

  
  const stepValues: StepValues = iassign(emptyStepValues(),
    x => x.resources["Basic"]["Temp"], x => 1);
  
  const newValues = triggerEffects([consumeEffect])(stepValues);
  
  console.log(newValues);
}

function test4() {
  const addModifierEffect: NodeEffect = {
    tag: "AddModifier",
    modifier: {
      charges: 1,
      chargePerUse: 1,
      maxCharges: 1,
      fragile: false,
      modifierEffect: {
        tag: "DoubleNextGain",
      },
    }
  }

  const stepValues: StepValues = emptyStepValues();
  
  const newValues = triggerEffects([addModifierEffect])(stepValues);
  
  console.log(newValues);
}

function test5() {
  const convertEffect: NodeEffect = {
    tag: "ConvertEffect",
    converts: {
      tag: "ConvertBothUnit",
      from: {
        color: "Basic",
        type: "Both",
      },
      to: {
        color: "Victory",
        type: "Total",
      },
      amount: "All",
    }
  }

  const stepValues: StepValues = iassign(iassign(emptyStepValues(),
  x => x.resources["Basic"]["Temp"], x => 1),
  x => x.resources["Basic"]["Total"], x => 1);
  
  const newValues = triggerEffects([convertEffect])(stepValues);
  
  console.log(newValues);
}

// loss takes temp first

function test6() {
  const lossEffect: NodeEffect = {
    tag: "LoseEffect",
    loss: {
      color: "Basic",
      type: "Both",
      amount: 1,
    }
  }

  const stepValues: StepValues = iassign(iassign(emptyStepValues(),
  x => x.resources["Basic"]["Temp"], x => 1),
  x => x.resources["Basic"]["Total"], x => 1);
  
  const newValues = triggerEffects([lossEffect])(stepValues);
  
  console.log(newValues);
}

// loss with buffer

function test7() {
  const lossEffect: NodeEffect = {
    tag: "LoseEffect",
    loss: {
      color: "Basic",
      type: "Both",
      amount: 1,
    }
  }

  const buffer: Modifier = {
    charges: 1,
    chargePerUse: 1,
    maxCharges: 1,
    fragile: false,
    modifierEffect: {
      tag: "Buffer",
      value: 10,
    },
  }

  const stepValues: StepValues = iassign(iassign(iassign(emptyStepValues(),
  x => x.resources["Basic"]["Temp"], x => 1),
  x => x.resources["Basic"]["Total"], x => 1),
  x => x.modifiers, x => [buffer]);
  
  const newValues = triggerEffects([lossEffect])(stepValues);
  
  console.log(JSON.stringify(newValues));
}

// destroy mod

function test8() {
  const destroyModEffect: NodeEffect = {
    tag: "DestroyModEffect",
    position: 0,
  }

  const buffer: Modifier = {
    charges: 1,
    chargePerUse: 1,
    maxCharges: 1,
    fragile: false,
    modifierEffect: {
      tag: "Buffer",
      value: 10,
    },
  }

  const stepValues: StepValues = iassign(iassign(iassign(emptyStepValues(),
  x => x.resources["Stack"]["Temp"], x => 1),
  x => x.resources["Stack"]["Total"], x => 1),
  x => x.modifiers, x => [buffer, buffer]);

  const newValues = triggerEffects([destroyModEffect])(stepValues);
  
  console.log(JSON.stringify(newValues));
}

test8();