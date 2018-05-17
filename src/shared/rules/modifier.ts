import { NodeEffect } from "src/shared/rules/effect";
import iassign from "immutable-assign";

export type Modifier = {
  charges: number,
  chargePerUse: number,
  modifierEffect: ModifierEffect,
};

type IgnoreNextConsume = {
  tag: "IgnoreNextConsume",
};

type IgnoreNextCheck = {
  tag: "IgnoreNextCheck",
};

type DoubleNextGain = {
  tag: "DoubleNextGain",
};


export type ModifierEffect
  = IgnoreNextConsume
  | IgnoreNextCheck
  | DoubleNextGain
  ;

export function modifierFunction(modifier: Modifier):
  (ne: NodeEffect) => { newEffect: NodeEffect, newModifiers: Modifier[] } {
    return nodeEffect => {
      const modifierChargeReduced: Modifier = iassign(modifier,
        m => m.charges, c => c - modifier.chargePerUse);
      const newModifiers: Modifier[] = modifierChargeReduced.charges > 0 ?
        [modifierChargeReduced] : [];
      switch (modifier.modifierEffect.tag) {
        case "IgnoreNextConsume": {
          return { newEffect: nodeEffect, newModifiers: newModifiers };
        }
        case "IgnoreNextCheck": {
          return { newEffect: nodeEffect, newModifiers: newModifiers };
        }
        case "DoubleNextGain": {
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.gains, g => g.map(x => iassign(x, x => x.amount, x => x * 2)));
              return { newEffect: nodeEffect, newModifiers: newModifiers };
            }
            default: {
              return { newEffect: nodeEffect, newModifiers: newModifiers };
            }
          }
        }
      }
    };
  }