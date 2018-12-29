import { focus, over, set } from "src/shared/iassign-util";
import { UnitId, overUnit, overFriendly, killUnit, getUnit, posToString } from "./entityId";
import { GameState, FrStUnit } from "./state";
import { addThreat } from "./threat";
import { Trigger, loseFragments, addFragments, Armor } from "./trigger";
import { damage, heal, useCharge } from "./unit";

export class Damage {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "Damage" = "Damage",
  ) {}
}

export class Heal {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "Heal" = "Heal",
  ) {}
}

export class UseCharge {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly tag: "UseCharge" = "UseCharge",
  ) {}
}

export class CombinedAction {
  constructor(
    public readonly actions: Action[],
    public readonly tag: "CombinedAction" = "CombinedAction",
  ) {}
}

export class AddThreat {
  constructor(
    public readonly toFriendly: UnitId,
    public readonly atEnemy: UnitId,
    public readonly value: number,
    public readonly tag: "AddThreat" = "AddThreat",
  ) {}
}

export class AddTrigger {
  constructor(
    public readonly target: UnitId,
    public readonly trigger: Trigger,
    public readonly tag: "AddTrigger" = "AddTrigger",
  ) {}
}

export class LoseFragments {
  constructor(
    public readonly target: UnitId,
    public readonly value: number,
    public readonly triggerTag: Trigger["tag"],
    public readonly triggerType: Trigger["type"],
    public readonly tag: "LoseFragments" = "LoseFragments",
  ) {}
}

export class Death {
  constructor(
    public readonly target: UnitId,
    public readonly tag: "Death" = "Death",
  ) {}
}

export class Invalid {
  constructor(
    public readonly tag: "Invalid" = "Invalid",
  ) {}
}

export class SwapHPWithExcess {
  constructor(
    public readonly target1: UnitId,
    public readonly target2: UnitId,
    public readonly tag: "SwapHPWithExcess" = "SwapHPWithExcess",
  ) {}
}

export class StartTurn {
  constructor(
    public readonly tag: "StartTurn" = "StartTurn",
  ) {}
}

export type Action
  = Damage
  | Heal
  | UseCharge
  | CombinedAction
  | AddThreat
  | AddTrigger
  | LoseFragments
  | Death
  | Invalid
  | SwapHPWithExcess
  | StartTurn
  ;

export function applyAction(
  action: Action,
  state: GameState,
): {
  state: GameState,
  actions: Action[],
} {
  if (state.state === "invalid" || state.state === "win") {
    return {
      state,
      actions: [],
    }
  }
  switch (action.tag) {
    case "Damage": {
      state = overUnit(action.target,
        state,
        x => damage(x, action.value),
        x => x,
      );
      const unit = getUnit(action.target, state);
      let actions: Action[] = [];
      if (unit !== undefined && unit.hp <= 0) {
        actions = [new Death(action.target)];
      }
      return {
        state,
        actions,
      };
    }
    case "Heal": {
      return {
        state: overUnit(action.target,
          state,
          x => heal(x, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "UseCharge": {
      return {
        state: overUnit(action.target,
          state,
          x => useCharge(x, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "CombinedAction": {
      return {
        state,
        actions: action.actions,
      }
    }
    case "AddThreat": {
      return {
        state: overFriendly(action.toFriendly,
          state,
          x => addThreat(x, state, action.atEnemy, action.value),
          x => x,
        ),
        actions: [],
      };
    }
    case "AddTrigger": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.triggers[action.trigger.type], x => addFragments(x, action.trigger))),
          x => x,
        ),
        actions: [],
      };
    }
    case "LoseFragments": {
      return {
        state: overUnit(action.target,
          state,
          x => focus(x, over(x => x.triggers[action.triggerType], x => loseFragments(x, action.triggerTag, action.value))),
          x => x,
        ),
        actions: [],
      };
    }
    case "Death": {
      const unit = getUnit(action.target, state);
      if (unit !== undefined && action.target.type === "friendly" && (<FrStUnit>unit).vital === true) {
        return {
          state: killUnit(action.target, state),
          actions: [new Invalid],
        }
      } else {
        return {
          state: killUnit(action.target, state),
          actions: [],
        }
      }
    }
    case "Invalid": {
      return {
        state: focus(state, set(x => x.state, "invalid")),
        actions: [],
      }
    }
    case "SwapHPWithExcess": {
      if (action.target1.type === "enemy" || action.target2.type === "enemy") {
        return {
          state,
          actions: [new Invalid],
        }
      }
      const unit1 = getUnit(action.target1, state);
      const unit2 = getUnit(action.target2, state);
      if (unit1 !== undefined && unit2 !== undefined) {
        const newUnit1Hp = unit2.hp > unit1.maxHp ? unit1.maxHp : unit2.hp;
        const newUnit2Hp = unit1.hp > unit2.maxHp ? unit2.maxHp : unit1.hp;

        state = overUnit(action.target1,
          state,
          x => focus(x, set(x => x.hp, newUnit1Hp)),
          x => x,
        );
        state = overUnit(action.target2,
          state,
          x => focus(x, set(x => x.hp, newUnit2Hp)),
          x => x,
        );

        const newUnit1Armor = unit2.hp > unit1.maxHp ?
          new AddTrigger(action.target1, new Armor((unit2.hp - unit1.maxHp) * 100, "other")) : undefined;
        const newUnit2Armor = unit1.hp > unit2.maxHp ?
          new AddTrigger(action.target2, new Armor((unit1.hp - unit2.maxHp) * 100, "other")) : undefined;

        return {
          state,
          actions: <Action[]>([newUnit1Armor, newUnit2Armor].filter(x => x !== undefined)),
        }
      }
      return {
        state,
        actions: [],
      }
    }
    case "StartTurn": {
      return {
        state,
        actions: [],
      }
    }
  }
}


export function actionText(
  action: Action
): string {
  switch (action.tag) {
    case "Damage": {
      return `DMG ${action.value} to ${posToString(action.target)}`;
    }
    case "Heal": {
      return `HEAL ${action.value} to ${posToString(action.target)}`;
    }
    case "UseCharge": {
      return `USE ${action.value} CH to ${posToString(action.target)}`;
    }
    case "CombinedAction": {
      const texts = action.actions.map(actionText);
      return texts.join(" && ");
    }
    case "AddThreat": {
      return `ADD ${action.value} TH to ${posToString(action.toFriendly)} at ${posToString(action.atEnemy)}`
    }
    case "AddTrigger": {
      return `+STA ${action.trigger.fragments} ${action.trigger.tag} to ${posToString(action.target)}`;
    }
    case "LoseFragments": {
      return `-STA ${action.value} ${action.triggerTag} to ${posToString(action.target)}`;
    }
    case "Death": {
      return `DEATH ${posToString(action.target)}`;
    }
    case "Invalid": {
      return `INVALID`;
    }
    case "SwapHPWithExcess": {
      return `SWAP HP with ${posToString(action.target1)} and ${posToString(action.target2)}, excess is armor`;
    }
    case "StartTurn": {
      return `StartTurn`;
    }
  }
}