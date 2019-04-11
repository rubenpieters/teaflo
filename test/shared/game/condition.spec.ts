import { resolveCondition } from "../../../src/shared/game/condition";
import { EntityId, statusId, friendlyId } from "../../../src/shared/definitions/entityId";
import { StStatus } from "../../../src/shared/definitions/statusRow";
import { ActionCondition, Var, statusOwner } from "../../../src/shared/definitions/condition";
import { Damage } from "../../../src/shared/definitions/actionf";
import { Action } from "../../../src/shared/definitions/action";
import { Weak } from "../../../src/shared/definitions/status";

const cond1: ActionCondition = {
  ...new Damage("Cond", "Cond", new Var("1"), statusOwner()),
  origin: new Var("2")
  };
const action1: Action = new Damage("Action", "Action", 1, new EntityId(0, "friendly"));
const status1: StStatus = {...new Weak(1), id: statusId(1), owner: friendlyId(0) };

const test = resolveCondition(status1, cond1, action1);

console.log(JSON.stringify(test));

const status2: StStatus = {...new Weak(1), id: statusId(1), owner: friendlyId(1) };
const test2 = resolveCondition(status2, cond1, action1);

console.log(JSON.stringify(test2));