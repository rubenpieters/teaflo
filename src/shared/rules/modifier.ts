import { NodeEffect } from "src/shared/rules/effect";
import iassign from "immutable-assign";

export type Modifier = {
  charges: number,
  chargePerUse: number,
  maxCharges: number,
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

type Buffer = {
  tag: "Buffer",
  value: number,
}

export type ModifierEffect
  = IgnoreNextConsume
  | IgnoreNextCheck
  | DoubleNextGain
  | Buffer
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
              return { newEffects: modifiedEffect, newModifiers: modifiersAfterUse };
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
              return { newEffects: modifiedEffect, newModifiers: modifiersAfterUse };
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
        case "Buffer": {
          switch (nodeEffect.tag) {
            case "LoseEffect": {
              const loseAmount: number = nodeEffect.loss.amount;
              if (modifier.modifierEffect.value > loseAmount) {
                const modifiedEffect = iassign(nodeEffect,
                  x => x.loss.amount, x => 0);
                const newModifier: Modifier = iassign(modifier,
                  x => (<Buffer>x.modifierEffect).value, x => x - nodeEffect.loss.amount);
                return { newEffects: [modifiedEffect], newModifiers: [newModifier] };
              } else {
                const modifiedEffect = iassign(nodeEffect,
                  x => x.loss.amount, x => x - (<Buffer>modifier.modifierEffect).value);
                return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
              }
            }
            default: {
              return { newEffects: [nodeEffect], newModifiers: [modifier] };
            }
          }
        }
      }
    };
  }

  export function loseCharge(modifier: Modifier): Modifier | undefined {
    const newModifier = iassign(modifier,
      x => x.charges, x => x - modifier.chargePerUse);
    if (newModifier.charges <= 0) {
      return undefined;
    } else {
      return newModifier;
    }
  }

  export function refreshCharge(modifier: Modifier): Modifier {
    const newModifier = iassign(modifier,
      x => x.charges, x => modifier.maxCharges);
      return newModifier;
  }