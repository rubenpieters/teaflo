import { NodeEffect } from "src/shared/rules/effect";
import { ResourceColor } from "src/shared/rules/resource";
import iassign from "immutable-assign";

export type Modifier = {
  charges: number,
  chargePerUse: number,
  maxCharges: number,
  modifierEffect: ModifierEffect,
  fragile: boolean,
};

type NoopMod = {
  tag: "NoopMod",
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
};

type Persister = {
  tag: "Persister",
  cap: number,
};

type IncreaseLoss = {
  tag: "IncreaseLoss",
  value: number,
};

type IncreaseGain = {
  tag: "IncreaseGain",
  value: number,
};

type DuplicateAddMod = {
  tag: "DuplicateAddMod",
  value: number,
};

type LossToGain = {
  tag: "LossToGain",
};

type GainXToVictory = {
  tag: "GainXToVictory",
  minimum: number,
  type: ResourceColor,
};

export type ModifierEffect
  = NoopMod
  | IgnoreNextConsume
  | IgnoreNextCheck
  | DoubleNextGain
  | Buffer
  | Persister
  | IncreaseGain
  | IncreaseLoss
  | DuplicateAddMod
  | LossToGain
  | GainXToVictory
  ;

export function showModifier(modifier: Modifier): string {
  const showCharges: string = "[" +
      modifier.charges + "/" +
      modifier.maxCharges + " (" +
      modifier.chargePerUse + ")]" +
      (modifier.fragile ? " F" : "");
  switch (modifier.modifierEffect.tag) {
    case "NoopMod": {
      return "TODO";
    }
    case "IgnoreNextConsume": {
      return "TODO";
    }
    case "IgnoreNextCheck": {
      return "TODO";
    }
    case "DoubleNextGain": {
      return "TODO";
    }
    case "Buffer": {
      return "Buffer " + modifier.modifierEffect.value + " " + showCharges;
    }
    case "Persister": {
      return "Persister " + modifier.modifierEffect.cap + " " + showCharges;
    }
    case "IncreaseGain": {
      return "IncreaseGain " + modifier.modifierEffect.value + " " + showCharges;
    }
    case "IncreaseLoss": {
      return "IncreaseLoss " + modifier.modifierEffect.value + " " + showCharges;
    }
    case "DuplicateAddMod": {
      return "DuplicateAddMod " + modifier.modifierEffect.value + " " + showCharges;
    }
    case "LossToGain": {
      return "LossToGain " + showCharges;
    }
    case "GainXToVictory": {
      return "Gain " + modifier.modifierEffect.minimum + " " + modifier.modifierEffect.type + " To Victory " + showCharges;
    }
  }
}

export function modifierFunction(modifier: Modifier):
  (ne: NodeEffect) => { newEffects: NodeEffect[], newModifiers: Modifier[] } {
    return nodeEffect => {
      const modifierChargeReduced: Modifier = iassign(modifier,
        m => m.charges, c => c - modifier.chargePerUse);
      const modifiersAfterUse: Modifier[] = modifierChargeReduced.charges > 0 ?
        [modifierChargeReduced] : [];

      const unmodifiedResult = { newEffects: [nodeEffect], newModifiers: [modifier] };
      const modifiedResult = { newEffects: [nodeEffect], newModifiers: [modifierChargeReduced] };
      const afterNoTrigger = modifier.fragile ? modifiedResult : unmodifiedResult;
      switch (modifier.modifierEffect.tag) {
        case "NoopMod": {
          return afterNoTrigger;
        }
        case "IgnoreNextConsume": {
          switch (nodeEffect.tag) {
            case "ConsumeEffect": {
              const modifiedEffect = nodeEffect.afterConsume;
              return { newEffects: modifiedEffect, newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
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
              return afterNoTrigger;
            }
          }
        }
        case "DoubleNextGain": {
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.gain.amount, x => x * 2);
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
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
                return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
              } else {
                const modifiedEffect = iassign(nodeEffect,
                  x => x.loss.amount, x => x - (<Buffer>modifier.modifierEffect).value);
                return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
              }
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "Persister": {
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const gainAmount: number = nodeEffect.gain.amount;
              if (gainAmount > modifier.modifierEffect.cap || nodeEffect.gain.type === "Total") {
                return { newEffects: [nodeEffect], newModifiers: [modifier] };
              } else {
                const modifiedEffect = iassign(nodeEffect,
                  x => x.gain.type, x => "Total");
                  return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
              }
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "IncreaseLoss": {
          const incValue: number = modifier.modifierEffect.value;
          switch (nodeEffect.tag) {
            case "LoseEffect": {
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.loss.amount, x => x + incValue);
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "IncreaseGain": {
          const incValue: number = modifier.modifierEffect.value;
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.gain.amount, x => x + incValue);
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "DuplicateAddMod": {
          switch (nodeEffect.tag) {
            case "AddModifier": {
              const duplicationAmount = modifier.modifierEffect.value;
              const modifiedEffect: NodeEffect = iassign(nodeEffect,
                x => x.amount, x => duplicationAmount + 1);
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "LossToGain": {
          switch (nodeEffect.tag) {
            case "LoseEffect": {
              const modifiedEffect: NodeEffect = {
                tag: "GainEffect",
                gain: {
                  color: nodeEffect.loss.color,
                  type: "Temp",
                  amount: nodeEffect.loss.amount,
                },
              };
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
            }
          }
        }
        case "GainXToVictory": {
          switch (nodeEffect.tag) {
            case "GainEffect": {
              const modifiedEffect: NodeEffect = {
                tag: "GainEffect",
                gain: {
                  color: "Victory",
                  type: "Total",
                  amount: 1,
                },
              };
              return { newEffects: [modifiedEffect], newModifiers: modifiersAfterUse };
            }
            default: {
              return afterNoTrigger;
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

  function fillArray<A>(value: A, len: number): A[] {
    const arr = new Array(len);
    for (let i = 0; i < len; i++) {
      arr[i] = value;
    }
    return arr;
  }