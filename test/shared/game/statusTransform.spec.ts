import { resolveStatusTransform } from "../../../src/shared/game/statusTranform";
import { Damage } from "../../../src/shared/definitions/actionf";
import { statusOwner } from "../../../src/shared/definitions/condition";
import { EntityId, friendlyId, enemyId, statusId } from "../../../src/shared/definitions/entityId";
import { Weak } from "../../../src/shared/definitions/status";
import { StatusTransform, Var } from "../../../src/shared/definitions/statusTransform";

const trans1: StatusTransform = {
  ...new Damage("ST", "ST", new Var("1"), statusOwner()),
  origin: new Var("2"),
};

const test = resolveStatusTransform(trans1, { "1": 2, "2": friendlyId(0) }, {
    ...new Weak(1),
    id: statusId(2),
    owner: friendlyId(0),
  }, {
    ...new Damage("Action", "Action", 1, enemyId(1)),
    origin: friendlyId(0),
  }
);

console.log(JSON.stringify(test));