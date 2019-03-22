import { mkGameState } from "../../../src/shared/game/state";
import { triggerArbitrary } from "../../../src/shared/game/trigger";
import fc from "fast-check";
import deepequal from "deep-equal";
import expect from "expect";

const neededDueToCompilationOrder = () => mkGameState(<any>undefined,<any>undefined);



const res = fc.assert(fc.property(triggerArbitrary, (t) => deepequal(t, t)));

console.log(res);

//console.log(fc.sample(triggerArbitrary));
