import { ActionCondition, Var, statusOwner, resolveCondition } from "../../../src/shared/game/condition";
import { Damage, Action } from "../../../src/shared/game/action";
import { EntityId, statusId, friendlyId } from "../../../src/shared/game/entityId";
import { Weak } from "../../../src/shared/game/status";
import { StStatus } from "../../../src/shared/game/statusRow";

const cond1: ActionCondition = new Damage("Cond", "Cond", new Var("1"), statusOwner());
const action1: Action = new Damage("Action", "Action", 1, new EntityId(0, "friendly"));
const status1: StStatus = {...new Weak(1), id: statusId(1), owner: friendlyId(0) };

const test = resolveCondition(status1, cond1, action1);

console.log(JSON.stringify(test));

const status2: StStatus = {...new Weak(1), id: statusId(1), owner: friendlyId(1) };
const test2 = resolveCondition(status2, cond1, action1);

console.log(JSON.stringify(test2));