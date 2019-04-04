import { ActionCondition, Var, statusOwner, resolveCondition } from "../../../src/shared/game/condition";
import { Damage, Action } from "../../../src/shared/game/action";
import { EntityId } from "../../../src/shared/game/entityId";
import { StatusContext } from "../../../src/shared/game/context";

const cond1: ActionCondition = new Damage("Cond", "Cond", new Var(), statusOwner());
const action1: Action = new Damage("Action", "Action", 1, new EntityId(0, "friendly"));

const test = resolveCondition(<any>undefined, cond1, action1, new StatusContext(new EntityId(0, "friendly")));

console.log(JSON.stringify(test));


const test2 = resolveCondition(<any>undefined, cond1, action1, new StatusContext(new EntityId(1, "friendly")));

console.log(JSON.stringify(test2));