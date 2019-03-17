import { focus, over, set } from "../iassign-util";
import { UnitId, overUnit, overFriendly, killUnit, getUnit, posToString, findIndex, TargetId, toGlobalId, UnitType, GlobalId, getStatus, killStatus, overEnemy } from "./entityId";
import { GameState, FrStUnit, findStatus } from "./state";
import { addThreat } from "./threat";
import { Trigger, loseFragments, addFragments, Armor, StTrigger, HasOwner } from "./trigger";
import { damage, heal, useCharge } from "./unit";
import { HasId } from "./hasId";
import { nextAI } from "./ai";

export class Damage {
  constructor(
    public readonly target: TargetId,
    public readonly value: number,
    public readonly tag: "Damage" = "Damage",
  ) {}
}

export class Heal {
  constructor(
    public readonly target: TargetId,
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

export class Death {
  constructor(
    public readonly target: TargetId,
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

export class NextAI {
  constructor(
    public readonly target: GlobalId<"enemy">,
    public readonly tag: "NextAI" = "NextAI",
  ) {}
}

export type Action
  = Damage
  | Heal
  | UseCharge
  | CombinedAction
  | AddThreat
  | AddTrigger
  | Death
  | Invalid
  | SwapHPWithExcess
  | StartTurn
  | NextAI
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
      const target = action.target;
      let actions: Action[] = [];
      if (target.type === "status") {
        const statusIndex = findStatus(state, target);
        if (statusIndex === undefined) {
          throw `findStatus: id ${target.id} not found`;
        }
        const value = action.value;
        state = focus(state,
          over(x => x.triggers[statusIndex.group], x => {
            return focus(x,
              over(x => x[statusIndex.index].fragments, x => x - value),
            );
          }),
        );
        const status = getStatus(target, state);
        if (status === undefined) {
          throw `findStatus: id ${target.id} not found`;
        }
        if (status.fragments <= 0) {
          actions = [new Death(target)];
        }
      } else {
        state = overUnit(target,
          state,
          x => damage(x, action.value),
          x => x,
        ); 
        const unit = getUnit(target, state);
        if (unit !== undefined && unit.hp <= 0) {
          actions = [new Death(target)];
        }
      }
      return {
        state,
        actions,
      };
    }
    case "Heal": {
      const target = action.target;
      if (target.type === "status") {
        const statusIndex = findStatus(state, target);
        if (statusIndex === undefined) {
          throw `findStatus: id ${target.id} not found`;
        }
        const value = action.value;
        state = focus(state,
          over(x => x.triggers[statusIndex.group], x => {
            return focus(x,
              over(x => x[statusIndex.index].fragments, x => x + value),
            );
          }),
        );
      } else {
        state = overUnit(target,
          state,
          x => heal(x, action.value),
          x => x,
        );
      }
      return {
        state,
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
      const globalId = toGlobalId(state, action.target);
      const stTrigger: Trigger & HasOwner = {...action.trigger, ...{ owner: globalId } };
      state = addFragments(state, stTrigger);
      return {
        state,
        actions: [],
      };
    }
    case "Death": {
      const target: TargetId = action.target;
      if (target.type === "status") {
        return {
          state: killStatus(target, state),
          actions: [],
        }
      } else {
        const unit = getUnit(target, state);
        if (unit !== undefined && target.type === "friendly" && (<FrStUnit>unit).vital === true) {
          return {
            state: killUnit(target, state),
            actions: [new Invalid],
          }
        } else {
          return {
            state: killUnit(target, state),
            actions: [],
          }
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
          new AddTrigger(action.target1, new Armor((unit2.hp - unit1.maxHp) * 100)) : undefined;
        const newUnit2Armor = unit1.hp > unit2.maxHp ?
          new AddTrigger(action.target2, new Armor((unit1.hp - unit2.maxHp) * 100)) : undefined;

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
    case "NextAI": {
      return {
        state: overEnemy(action.target,
          state,
          x => nextAI(state, x, action.target),
          x => x,
        ),
        actions: [],
      };
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
    case "NextAI": {
      return `NEXTAI ${posToString(action.target)}`;
    }
  }
}

export function ignoreTag(
  tag: Action["tag"],
): boolean {
  return tag === "CombinedAction";
}