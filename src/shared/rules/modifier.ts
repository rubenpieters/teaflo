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
  (ne: NodeEffect) => { newEffects: NodeEffect[], newModifiers: Modifier[] } {
    return nodeEffect => {
      const modifierChargeReduced: Modifier = iassign(modifier,
        m => m.charges, c => c - modifier.chargePerUse);
      const modifiersAfterUse: Modifier[] = modifierChargeReduced.charges > 0 ?
        [modifierChargeReduced] : [];
      switch (modifier.modifierEffect.tag) {
        case "IgnoreNextConsume": {
          switch (nodeEffect.tag) {
            case "ConsumeEffect": {
              const modifiedEffect = nodeEffect.afterConsume;
              return { newEffects: modifiedEffect, newModifiers: [modifier] };
            }
            default: {
              return { newEffects: [nodeEffect], newModifiers: [modifier] };
            }
          }
        }
        case "IgnoreNextCheck": {
          switch (nodeEffect.tag) {
            case "CheckEffect": {
              const modifiedEffect = nodeEffect.afterCheck;
              return { newEffects: modifiedEffect, newModifiers: [modifier] };
            }
            default: {
              return { newEffects: [nodeEffect], newModifiers: [modifier] };
            }
          }
        }
        case "DoubleNextGain": {
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.gains, g => g.map(x => iassign(x, x => x.amount, x => x * 2)));
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return { newEffects: [nodeEffect], newModifiers: [modifier] };
            }
          }
        }
      }
    };
  }