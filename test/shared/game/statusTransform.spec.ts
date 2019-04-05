import { StatusTransform, resolveStatusTransform } from "../../../src/shared/game/statusTranform";
import { Damage } from "../../../src/shared/game/action";
import { Var, statusOwner } from "../../../src/shared/game/condition";
import { StatusContext } from "../../../src/shared/game/context";
import { EntityId } from "../../../src/shared/game/entityId";
import { Weak } from "../../../src/shared/game/status";

const trans1: StatusTransform = new Damage("ST", "ST", new Var("1"), statusOwner());

const test = resolveStatusTransform(<any>undefined, trans1, new StatusContext(new EntityId(0, "friendly"), new Weak(1)), { "1": 2 });

console.log(JSON.stringify(test));